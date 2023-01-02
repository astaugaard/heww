#include <stdio.h>
#include <pthread.h>
#include <stdbool.h>
#include "volumeFunction.h"
#include <pulse/pulseaudio.h>
#include <string.h>
#include <math.h>

void success(pa_context *c, int success, void *userdat) {}

void changeVolumeCallback(pa_context *c, const pa_sink_info *i, int num, void *userdat) {
    float* f = (float*)userdat;
    if (i) {
        pa_cvolume v = i->volume;
        pa_volume_t volume = pa_cvolume_avg(&v);
        int32_t newVolume = ((int32_t)volume + (int32_t)((float)PA_VOLUME_NORM * *f));

        if (newVolume > (int32_t)PA_VOLUME_NORM) {
            newVolume = (int32_t)PA_VOLUME_NORM;
        } else if (newVolume < (int32_t)PA_VOLUME_MUTED) {
            newVolume = (int32_t)PA_VOLUME_MUTED;
        }

        pa_volume_t newVolume2 = (uint32_t)newVolume;

        pa_cvolume_set(&v, v.channels, newVolume2);

        pa_context_set_sink_volume_by_index(c, i->index, &v, success, userdat);
    }
    // somehow this is freed atomatically and having this here results in a double free
    // free(f);
}

void setVolumeCallback(pa_context *c, const pa_sink_info *i, int num, void *userdat) {
    float* f = (float*)userdat;
    if (i) {
        pa_cvolume v = i->volume;
        pa_volume_t newVolume = (int32_t)((float)PA_VOLUME_NORM * *f);
        pa_cvolume_set(&v,v.channels,newVolume);
        pa_context_set_sink_volume_by_index(c,i->index,&v,success,userdat);
    }
}

void setVolume(struct ListenerData* dat, float v) {
    if (!dat) {
        fprintf(stderr, "default sink name not yet fetched");
        return;
    };
    float* f = malloc(sizeof (float));
    *f = v;
    pa_context_get_sink_info_by_name(dat->context,dat->defaultSink,changeVolumeCallback,f);
}

void changeVolumeBy(struct ListenerData* dat, float v) {
    if (!dat) {
        fprintf(stderr, "default sink name not yet fetched");
        return;
    };
    float* f = malloc(sizeof (float));
    *f = v;
    pa_context_get_sink_info_by_name(dat->context,dat->defaultSink,changeVolumeCallback,f);
}

// c version of https://gist.github.com/jasonwhite/1df6ee4b5039358701d2
// with modifications to be used in a bigger program

static void cleanUp2(struct ListenerData* dat) {
    if (dat->context) {
        pa_context_unref(dat->context);
        dat->context = NULL;
    }

    if (dat->mainloop) {
        pa_threaded_mainloop_free(dat->mainloop);
        dat->mainloop = NULL;
        dat->mainloop_api = NULL;
    }

    if (dat->defaultSink) {
        free(dat->defaultSink);
        dat->defaultSink = NULL;
    }
};

static void infoCallback(pa_context *c, const pa_sink_info *i, int num, void *userdat) {
    struct ListenerData* dat = (struct ListenerData*)userdat;

    if (i) {
        float volume = (float)pa_cvolume_avg(&(i->volume)) / (float)PA_VOLUME_NORM;
        dat->volume_callback(i->mute, volume * 100.0f);
    }
};

static void sink_update_callback(pa_context *c, pa_subscription_event_type_t type, uint32_t idx, void *userdat) {
    unsigned facility = type & PA_SUBSCRIPTION_EVENT_FACILITY_MASK;

    pa_operation* op = NULL;
    switch (facility) {
        case PA_SUBSCRIPTION_EVENT_SINK:
            op = pa_context_get_sink_info_by_index(c, idx, &infoCallback, userdat);
            break;
        default:
            assert(0);
            break;
    }
    if (op) {
        pa_operation_unref(op);
    }
};

static void server_info_callback(pa_context *c, const pa_server_info *i, void *userdat) {
    struct ListenerData* dat = (struct ListenerData*)userdat;
    dat->defaultSink = strdup(i->default_sink_name);
    printf("default sink name = %s\n", i->default_sink_name);
    pa_context_get_sink_info_by_name(c, i->default_sink_name, infoCallback, userdat);
};

static void connectionCallback(pa_context *c, void *userdat) {
    assert (c && userdat);

    switch (pa_context_get_state(c)) 
    {
        case PA_CONTEXT_CONNECTING:
        case PA_CONTEXT_AUTHORIZING:
        case PA_CONTEXT_SETTING_NAME:
            break;
        case PA_CONTEXT_READY:
            printf("PulseAudio connection established\n");
            pa_context_get_server_info(c,server_info_callback,userdat);
            pa_context_set_subscribe_callback(c, sink_update_callback, userdat);
            pa_context_subscribe(c, PA_SUBSCRIPTION_MASK_SINK, NULL, NULL);
            break;
    }
};

static struct ListenerData* init(v_handle_callback cb) {
    struct ListenerData* dat = malloc(sizeof (struct ListenerData));
    dat->mainloop = NULL;
    dat->mainloop_api = NULL;
    dat->context = NULL;
    dat->defaultSink = NULL;

    dat->mainloop = pa_threaded_mainloop_new();

    dat->volume_callback = cb;

    if (!dat->mainloop) {
        fprintf(stderr, "pa_mainloop_new() failed\n");
        return NULL;
    }

    dat->mainloop_api = pa_threaded_mainloop_get_api(dat->mainloop);

    if (pa_signal_init(dat->mainloop_api) != 0) {
        fprintf(stderr, "pa_signal_init() failed\n");
        return NULL;
    }

    dat->context = pa_context_new(dat->mainloop_api, "pulseaudio volume listener");

    if (!dat->context) {
        fprintf(stderr, "pa_context_new() failed\n");
        return NULL;
    }

    if (pa_context_connect(dat->context,NULL,PA_CONTEXT_NOAUTOSPAWN,NULL) < 0) {
        fprintf(stderr, "pa_context_connect() failed with: %s\n", pa_strerror(pa_context_errno(dat->context)));
        return NULL;
    }

    pa_context_set_state_callback(dat->context, connectionCallback, dat);
    return dat;
};

struct ListenerData* run_volume_handle(v_handle_callback cb) {
    struct ListenerData* dat = init(cb);

    if (!dat) {
        cleanUp2(dat);
        free(dat);
        return NULL;
    }

    if (pa_threaded_mainloop_start(dat->mainloop) != 0) {
       fprintf(stderr, "pa_mainloop_run() failed\n");
       cleanUp2(dat);
       free(dat);
       return NULL;
    }        

    return dat;
};

void close_handle(struct ListenerData* closeHandle) {
    if (closeHandle) {
        if (closeHandle->mainloop) {
            pa_threaded_mainloop_stop(closeHandle->mainloop);
            cleanUp2(closeHandle);
            free(closeHandle);
            closeHandle = NULL;
        }
    };
}
