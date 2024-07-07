cmake_minimum_required(VERSION 3.10)

# 设置项目名和版本号
project(tanglijun_hw1)

# 设置编译器标准（可选）
set(CMAKE_C_STANDARD 99)

# 包含头文件目录
include_directories(${PROJECT_SOURCE_DIR}/inc)

# 添加源文件
set(SOURCES
 src/main.c
 src/algebra.c
)

# 添加可执行文件
add_executable(tanglijun_hw1 ${SOURCES})