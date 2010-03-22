include ../include/include.mk

DOC_OPTS={dir,\"../doc\"}

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

edoc:
	$(ERL) -noshell -pa ../ebin \
		-eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
		-s init stop

test: all
	$(ERL) -detached -setcookie $(TESTCOOKIE) -sname test1@localhost -pa ../ebin
	$(ERL) -detached -setcookie $(TESTCOOKIE) -sname test2@localhost -pa ../ebin
	sleep 1
	$(ERL) -noshell -setcookie $(TESTCOOKIE) -sname testmain@localhost -pa ../ebin -s test test
