all: install_rebar3 install

install_rebar3:
	@./config/install_rebar3.sh

install:
	@./config/rebar3 ck

reset:
	@./config/rebar3 reset

edoc:
	@./config/rebar3 edoc

upver:
	@PYTHONPATH=${PWD} python3 ./config/upver.py
