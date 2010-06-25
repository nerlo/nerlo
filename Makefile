all:
	(cd java; ant jar)
	(cd erl;$(MAKE) all)

