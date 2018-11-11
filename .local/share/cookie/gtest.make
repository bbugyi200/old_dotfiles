CPPFLAGS += -I /usr/include/gtest -I ../src
CXXFLAGS += -g -Wall -Wextra -pthread
CFLAGS = -DGOOGLE_TEST
LDLIBS = -lgtest -lboost_system -lboost_filesystem

build = ../
ofiles = $(addsuffix .o,$(filter-out ../src/main, $(basename $(wildcard ../src/*.c))))
ofiles += $(addsuffix .o,$(basename $(wildcard *.cc)))

$(build)/runtests: *.cc $(ofiles) | $(build)
	$(CXX) $(LDLIBS) -o $@ $^

$(build):
	@mkdir -p $(build)
