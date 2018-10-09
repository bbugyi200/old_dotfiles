#ifndef INCLUDED_{{ cookiecutter.project|upper }}
#define INCLUDED_{{ cookiecutter.project|upper }}

#include <{{ 'stdio.h' if cookiecutter.filetype == 'c' else 'gutils.h' }}>
{% if cookiecutter.filetype == 'c' %}#include <stdlib.h>{% endif %}
{% if cookiecutter.filetype == 'c' %}#include <string.h>{% endif %}
{% if cookiecutter.filetype == 'c' %}#include <unistd.h>{% endif %}

#endif /* INCLUDED_{{ cookiecutter.project|upper }} */
