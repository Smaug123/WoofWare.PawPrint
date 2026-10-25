cmake_minimum_required(VERSION 3.20)
project(native-call-graph LANGUAGES CXX)

find_package(Clang REQUIRED CONFIG)

if(NOT CLANG_RESOURCE_DIR)
  message(FATAL_ERROR "CLANG_RESOURCE_DIR must name the resource directory of the clang that compiles the units")
endif()

add_executable(native-call-graph callgraph.cpp)
target_compile_features(native-call-graph PRIVATE cxx_std_17)
target_include_directories(native-call-graph SYSTEM PRIVATE ${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
target_compile_definitions(native-call-graph PRIVATE CLANG_RESOURCE_DIR="${CLANG_RESOURCE_DIR}")
target_link_libraries(native-call-graph PRIVATE clang-cpp LLVM)
if(NOT LLVM_ENABLE_RTTI)
  target_compile_options(native-call-graph PRIVATE -fno-rtti)
endif()
install(TARGETS native-call-graph)
