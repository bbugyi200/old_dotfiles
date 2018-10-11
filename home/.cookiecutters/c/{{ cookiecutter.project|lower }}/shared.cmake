{% if cookiecutter.filetype == 'cc' %}function(setup_gutils target)
    # Setup glib
    find_package(PkgConfig REQUIRED)
    pkg_search_module(GLIB REQUIRED glib-2.0)
    target_include_directories(${target} PRIVATE ${GLIB_INCLUDE_DIRS})
    target_link_libraries(${target} ${GLIB_LDFLAGS})

    # Setup gutils
    target_link_libraries(${target} gutils)
endfunction(){% endif %}
