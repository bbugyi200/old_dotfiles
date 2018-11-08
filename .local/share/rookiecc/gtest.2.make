bin=../
build=../build
CPPFLAGS += -I /usr/include/gtest -I ../src
CXXFLAGS += -g -Wall -Wextra -pthread
CFILES=../src/[^main]*.c
OFILES=$(addsuffix .o,$(filter-out ../src/main, $(basename $(wildcard ../src/*.c))))

buildtests: *.cc bin ${CFILES} ${OFILES}
	g++ ${OFILES} ${CPPFLAGS} ${CXXFLAGS} -lgtest -o ${bin}/runtests *.cc

%.o: build %.c
	gcc -DGOOGLE_TEST -g -c $*.c -o $@

bin:
	[ -d ${bin} ] || mkdir ${bin}

build:
	[ -d ${build} ] || mkdir ${build}
