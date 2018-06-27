include(../shared.cmake)

include_directories(/usr/include/CppUTest)
include_directories(/usr/include/CppUTestExt)

# add cpputest library
add_library(imp_cpputest STATIC IMPORTED)
set_property(TARGET imp_cpputest PROPERTY
             IMPORTED_LOCATION /usr/lib/libCppUTest.a)

file(GLOB SOURCES "../src/[^main]*.cc")
file(GLOB TESTS "*.cc")
add_executable(runtests ${SOURCES} ${TESTS})
target_link_libraries(runtests imp_cpputest)
target_include_directories(runtests PUBLIC ../include)
