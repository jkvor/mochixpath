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

	XPath = "string(//dolphin)",
	Xml = "<xml><ocean><dolphin>i'm gonna eat a dalphin</dolphin></ocean></xml>",
	Subject = mochiweb_html:parse(Xml),
	etap:is(mochiweb_xpath:execute(XPath, Subject), "<dolphin>i'm gonna eat a dalphin</dolphin>", "xpath ok"),

	ok.