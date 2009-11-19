#!/usr/local/bin/escript
%% -*- erlang -*-
%%! -pa ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),
    case (catch start()) of
        {'EXIT', Err} ->
            io:format("# ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.
    
start() ->
    error_logger:tty(false),

	Xml = "<xml><ocean id='1'><dolphin>i'm gonna eat a dalphin</dolphin></ocean><ocean id='2'><squid>eyeball</squid></ocean></xml>",
	Subject = mochiweb_html:parse(Xml),
	etap:is(mochiweb_xpath:execute("string(//dolphin)", Subject), "<dolphin>i'm gonna eat a dalphin</dolphin>", "xpath ok"),
    etap:is(mochiweb_xpath:execute("//xml/ocean/squid/..", Subject), {<<"ocean">>, [{<<"id">>, "2"}], [<<"eyeball">>]}, "xpath ok"),
    
	ok.