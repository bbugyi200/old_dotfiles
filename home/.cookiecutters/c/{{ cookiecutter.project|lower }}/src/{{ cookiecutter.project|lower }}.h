/** @file {{ cookiecutter.project|lower }} */

#ifndef INCLUDED_{{ cookiecutter.project|upper }}
#define INCLUDED_{{ cookiecutter.project|upper }}

#include <{{ 'stdio.h' if cookiecutter.filetype == 'c' else 'gutils.h' }}>
{% if cookiecutter.filetype == 'c' %}#include <stdlib.h>{% endif %}
{% if cookiecutter.filetype == 'c' %}#include <string.h>{% endif %}
{% if cookiecutter.filetype == 'c' %}#include <unistd.h>{% endif %}

const {{ 'char*' if cookiecutter.filetype == 'c' else 'string' }} PROJECT_NAME = "{{ cookiecutter.project|lower }}";
{% if cookiecutter.filetype == 'c' -%}
extern bool debug_flag;

/************
*  Macros  *
************/
/** Debug messaging utility. */
#define DMSG(...) if(debug_flag) { \
    printf("[DEBUG] "); \
    printf(__VA_ARGS__); \
    printf("\n"); \
}

/** Evaluate expression value. */
#define DEVAL(x) DMSG("%s => %s", #x, x)

/** Error message. */
#define EMSG(...) \
    fprintf(stderr, "[ERROR] "); \
    fprintf(stderr, __VA_ARGS__); \
    fprintf(stderr, "\n");

/** Print an error message and then terminates the program.
 *
 * @param ec Exit code.
 * */
#define die(ec, ...) \
    EMSG(__VA_ARGS__) \
    exit(ec);
{%- endif %}
#endif /* INCLUDED_{{ cookiecutter.project|upper }} */
