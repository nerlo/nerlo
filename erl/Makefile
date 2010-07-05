all:
	(cd src;$(MAKE) all)
	(cd priv/log4erl-0.9.0;$(MAKE))
	(cd test;$(MAKE) all)

final: clean
	(cd src;$(MAKE) no_tests=true no_debug_info=true)
	
hipe: clean
	(cd src;$(MAKE) no_tests=true hipe=true no_debug_info=true)

edoc:
	(cd src;$(MAKE) edoc)

test: all
	(cd src;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	-rm *.dump


