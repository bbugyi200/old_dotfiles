vpath % ../src

override CFLAGS += -DGOOGLE_TEST
override CPPFLAGS +=
override LDFLAGS +=
override LDLIBS += -lstdc++ -lgtest -lboost_system -lboost_filesystem
CXXFLAGS += -I ../src -I /usr/include/gtest -pthread

override objects += $(subst .cc,.o,$(wildcard *.cc))
exe = runtests


$(exe): $(objects)
$(objects): $(project).h

.PHONY: run
run: $(exe)
	@./$(exe)

.PHONY: clean
clean:
	@rm -f $(exe)
