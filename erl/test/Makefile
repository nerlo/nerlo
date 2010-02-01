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
	-rm *.erank
	-rm tmp/*
	$(ERL) -noshell -setcookie 123 -sname test -pa ../ebin -s $(APPLICATION) test -s init stop
	
