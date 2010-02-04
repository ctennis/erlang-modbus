ERL = erl
EBIN = ebin
LIBDIR = $(shell erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell)
VERSION = $(shell cat VERSION | tr -d '\n')
CFLAGS = -pa $(EBIN) $(wildcard deps/*/ebin)
CC = $(ERL) $(CFLAGS)
APP_NAME = modbus
 
all: ebin compile
boot: all make_boot
start: all start_all
 
compile:
	@$(CC) -noinput +B -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'
 
edoc:
	@echo Generating $(APP_NAME) documentation from srcs
	@$(CC) -noinput -eval 'edoc:application($(APP), "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt
 
make_boot:
	(cd $(EBIN); $(CC) -noshell -run make_boot write_scripts $(APP_NAME))
 
start_all:
	(cd ebin; $(CC) -noshell -sname $(APP_NAME) -boot $(APP_NAME))
 
ebin:
	@mkdir ebin
 
clean:
	rm -rf ebin/*.beam ebin/erl_crash.dump erl_crash.dump ebin/*.boot ebin/*.rel ebin/*.script doc/*.html doc/*.css doc/erlang.png doc/edoc-info
