cmake_minimum_required(VERSION 3.20.0 FATAL_ERROR)
include(CMakeToolsHelpers OPTIONAL)

# Build type needs to be set as parameter to CMAKE: -DCMAKE_BUILD_TYPE=xxxx
# set(CMAKE_BUILD_TYPE Release)
# If the user specifies -DCMAKE_BUILD_TYPE on the command line, take their definition
# and dump it in the cache along with proper documentation, otherwise set CMAKE_BUILD_TYPE
# to Debug prior to calling PROJECT()
IF(DEFINED CMAKE_BUILD_TYPE)
   SET(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE} CACHE STRING "Choose the type of `build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
ELSE()
   SET(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build, options are: None(CMAKE_CXX_FLAGS or CMAKE_C_FLAGS used) Debug Release RelWithDebInfo MinSizeRel.")
ENDIF()

set(CMAKE_CXX_FLAGS_DEBUG  "-g")
set(CMAKE_CXX_FLAGS_DEBUG  "-ggdb")

project(indrascheme)

# include_directories(../..)

add_executable(indrascheme indrascheme.cpp indrascheme.h)
# add_executable (iltest test.cpp)

# set_property(TARGET iltest PROPERTY CXX_STANDARD 11)
set_property(TARGET indrascheme PROPERTY CXX_STANDARD 11)

