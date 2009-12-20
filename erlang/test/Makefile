all:
	cd src; make all

clean:
	cd src; make clean

test: all
	erl -noshell -pa ../target/main/ebin -pa ../target/test/ebin -noshell -s test test 
