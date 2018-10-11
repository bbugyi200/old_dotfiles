cmake_minimum_required(VERSION 2.8)
include(shared.cmake)
project({{ cookiecutter.project|lower }})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src)
add_subdirectory(test)
