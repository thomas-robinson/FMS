#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fms_diag_parser.h>



enum state_value {
    START,
    ACCEPT_SECTION,
    ACCEPT_LIST,
    ACCEPT_VALUES,
    ACCEPT_KEY,
    ACCEPT_VALUE,
    STOP,
    ERROR,
};


int consume_event(struct parser_state *s, yaml_event_t *event)
{
    s->accepted = 0;
    switch (s->state) {
    case START:
        switch (event->type) {
        case YAML_MAPPING_START_EVENT:
            s->state = ACCEPT_SECTION;
            break;
        case YAML_SCALAR_EVENT:
            fprintf(stderr, "Ignoring unexpected scalar: %s\n",
                    (char*)event->data.scalar.value);
            break;
        case YAML_SEQUENCE_START_EVENT:
            fprintf(stderr, "Unexpected sequence.\n");
            s->state = ERROR;
            break;
        case YAML_STREAM_END_EVENT: s->state = STOP; break;
        default:
            break;
        }
        break;
    case ACCEPT_SECTION:
        switch (event->type) {
        case YAML_SCALAR_EVENT:
            if (strcmp((char*)event->data.scalar.value, "files") == 0) {
               s->state = ACCEPT_LIST;
            } else {
               fprintf(stderr, "Unexpected scalar: %s\n",
                      (char*)event->data.scalar.value);
               s->state = ERROR;
            }
            break;
        default:
            fprintf(stderr, "Unexpected event while getting scalar: %d\n", event->type);
            s->state = ERROR;
            break;
        }
        break;
    case ACCEPT_LIST:
        switch (event->type) {
        case YAML_SEQUENCE_START_EVENT: s->state = ACCEPT_VALUES; break;
        default:
            fprintf(stderr, "Unexpected event while getting sequence: %d\n", event->type);
            s->state = ERROR;
            break;
        }
        break;
    case ACCEPT_VALUES:
        switch (event->type) {
        case YAML_MAPPING_START_EVENT:
            memset(&(s->data), 0, sizeof(s->data));
            s->state = ACCEPT_KEY;
            break;
        case YAML_SEQUENCE_END_EVENT: s->state = START; break;
        case YAML_DOCUMENT_END_EVENT: s->state = START; break;
        default:
            fprintf(stderr, "Unexpected event while getting mapped values: %d\n",
                    event->type);
            s->state = ERROR;
            break;
        }
        break;
    case ACCEPT_KEY:
        switch (event->type) {
        case YAML_SCALAR_EVENT:
            s->key = strdup((char*)event->data.scalar.value);
            s->state = ACCEPT_VALUE;
            break;
        case YAML_MAPPING_END_EVENT:
            s->accepted = 1;
            s->state = ACCEPT_VALUES;
            break;
        default:
            fprintf(stderr, "Unexpected event while getting key: %d\n",
                    event->type);
            s->state = ERROR;
            break;
        }
        break;
    case ACCEPT_VALUE:
        switch (event->type) {
        case YAML_SCALAR_EVENT:
            s->value = (char*)event->data.scalar.value;
            if (strcmp(s->key, "name") == 0) {
                s->data.name = strdup((char*)s->value);
            } else if (strcmp(s->key, "frequnit") == 0) {
                s->data.frequnit = strdup((char*)s->value);
            } else if (strcmp(s->key, "freq") == 0) {
                s->data.freq = atoi(s->value);
            } else if (strcmp(s->key, "timeunit") == 0) {
                s->data.timeunit = strdup((char*)s->value);
            } else if (strcmp(s->key, "unlimdim") == 0) {
                s->data.unlimdim = strdup((char*)s->value);
            } else {
                fprintf(stderr, "Ignoring unknown key: %s\n", s->key);
            }
            free(s->key);
            s->state = ACCEPT_KEY;
            break;
        default:
            fprintf(stderr, "Unexpected event while getting value: %d\n",
                    event->type);
            s->state = ERROR;
            break;
        }
        break;
    case ERROR:
    case STOP:
        break;
    }
    return (s->state == ERROR ? 0 : 1);
}

