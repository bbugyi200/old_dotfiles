bin=./bin
build=./build
project=simpleCSVsorter
sources=src/*.c
CFLAGS+=-g -std=c11

.PHONY: clean test 

$(project): $(bin) $(sources)
	$(CC) $(CFLAGS) -o $(bin)/$@ $(sources)

debug: $(bin) $(sources)
	$(CC) -fsanitize=address $(CFLAGS) -o $(bin)/$(project) $(sources)

$(bin):
	@mkdir -p $(bin)

clean:
	rm src/*.o
	if [ -d $(bin) ]; then \
		rm -f $(bin)/$(project) $(bin)/runtests; \
		if [ "$(bin)" != "./" ]; then rmdir $(bin); fi; \
	fi
	if [ -d $(build) ]; then \
		rm -f $(build)/*.o; \
		if [ "$(build)" != "./" ]; then rmdir $(build); fi; \
	fi

test:
	make -C test
