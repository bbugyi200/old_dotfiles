cmake_minimum_required(VERSION 2.8)
include(shared.cmake)
project({{ cookiecutter.project|lower }})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wextra")

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
   set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Werror")
endif()

add_subdirectory(src)
add_subdirectory(test)
