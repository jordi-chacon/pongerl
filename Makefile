include dep.inc

all:
	erl -make

init:
	-mkdir ebin
	-mkdir dep
	-git submodule init
	-git submodule update
	-(cd dep/cecho; make)
	-chmod +x ./start.sh
	cp src/pongerl.app.src ebin/pongerl.app 
	$(MAKE) all

clean:
	rm -rf ./ebin/*.beam
