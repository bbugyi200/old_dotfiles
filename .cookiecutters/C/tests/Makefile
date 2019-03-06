vpath % ../$(src)

override CFLAGS += -DGOOGLE_TEST
override CPPFLAGS +=
override LDFLAGS +=
override LDLIBS += -lstdc++ -lgtest -pthread
CXXFLAGS += -I ../$(src) -I /usr/include/gtest

override objects += $(subst .cc,.o,$(wildcard *.cc))
exe = runtests


$(exe): $(objects)

.PHONY: run
run: $(exe)
	@./$(exe)

.PHONY: clean
clean:
	@rm -f $(exe)
