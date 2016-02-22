all:
	rebar compile

run:
	ERL_LIBS=apps:deps erl +K true -instr -name probe@127.0.0.1 -pa ./ebin -boot start_sasl -eval "application:start(scout_probe)"  -sasl errlog_type error
