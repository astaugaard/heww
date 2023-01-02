#include <pulse/pulseaudio.h>

typedef void(*v_handle_callback)(bool muted, float volume);

struct ListenerData {
    pa_threaded_mainloop* mainloop;
    pa_mainloop_api* mainloop_api;
    pa_context* context;
    v_handle_callback volume_callback;
    char* defaultSink;
};

struct ListenerData* run_volume_handle(v_handle_callback cb);
void close_handle(struct ListenerData* closeHandle);
