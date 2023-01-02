#include <sensors/sensors.h>

char *sensors_get_feature_name(sensors_feature* f) {
    return f->name;
}

char *sensors_get_subfeature_name(sensors_subfeature* f) {
    return f->name;
}

int sensors_get_subfeature_number(sensors_subfeature* f) {
    return f->number;
}

int size_of_chipname = sizeof(sensors_chip_name);
