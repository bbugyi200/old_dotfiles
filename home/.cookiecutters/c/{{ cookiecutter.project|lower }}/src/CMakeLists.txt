file(GLOB SOURCES "*.{{ cookiecutter.filetype }}")
{% if cookiecutter.lib == 'y' %}
add_library({{ cookiecutter.project|lower }} STATIC ${SOURCES})
{% else %}
add_executable({{ cookiecutter.project|lower }} ${SOURCES})
{% endif %}
target_include_directories({{ cookiecutter.project|lower }} PUBLIC ../include)
{% if cookiecutter.filetype == 'cc' %}
target_link_libraries({{ cookiecutter.project|lower }} gutils){% endif %}
{% if cookiecutter.lib == 'y' %}
file(GLOB HEADERS "../include/*.h")
set_target_properties({{ cookiecutter.project|lower }} PROPERTIES PUBLIC_HEADER ${HEADERS})
install(TARGETS {{ cookiecutter.project|lower }}
        DESTINATION lib
        PUBLIC_HEADER DESTINATION include){% else %}
install(TARGETS {{ cookiecutter.project|lower }} DESTINATION bin){% endif %}
