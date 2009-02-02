%% 
%% @author Pablo Polvorin 
%% created on 2008-04-30
%% 
%% Some simple functional test cases, test the xpath implementation
-module(test).

%Tests
-export([test/0,test1/0,test2/0,test3/0,stupid/1]).


-define(HTML1, "html-docs/test1.html").
-define(HTML2, "html-docs/test2.html").


%% [{HtmlFileName,Cases}]
%% Cases = {XPath,ExpectedResult}
test_definitions() ->
    [ 
        {?HTML1,[
                {"/html/head/title/text()",[<<"Title">>]},
                {"/html/head/meta[2]/@content",[<<"text/html; charset=utf-8">>]},
                {"/html/body/div[@id='first']/@class",[<<"normal">>]},
                {"/html/body/form/input[@name='id3']/@value",[<<"Val3">>]},
                {"/html/body/*/input[@name='id3']/@value",[<<"Val3">>]},
                {"/html/head/title/text() = 'Title'",true},
                {"/html/head[non_existent/no_existent]/title/text()",[]}
                ]},
        {?HTML2,[
                {"/html/body/div[1]/a[3]/text()",[<<"ssddd">>]},
                {"/html/body/form[1]/input[@type='hidden']/@value",
                        [<<"Val1">>,<<"Val2">>,<<"Val3">>]},
                {"/html/body/form[input[@name='id1_2']/@value='Val1_2']/@action",
                    [<<"Action2">>]},
                {"//input[@name='id1_2']/@value",[<<"Val1_2">>]},
                {"//form[.//input[@name='id1_2']]/@action",[<<"Action2">>]},
                {"//form[.//input/@name = 'id1']/@action",[<<"Action1">>]},
                {"//form[//input/@name ='id1']/@action",
                                            [<<"Action1">>,<<"Action2">>]},
                {"count(/html/body/form[count(input[@type='hidden']) = 4])",
                                         1},
                {"name(/html/*)",<<"head">>},
                {"/html/body/form[starts-with(@action,'Act')]/@action",
                        [<<"Action1">>,<<"Action2">>]},
                {"//input[substring(@name,1,4) = 'id1_']/@value",
                        [<<"Val1_2">>]},
                {"//div[sum(number)=23]/@id",[<<"sum">>]},
                {"//div[sum(number)>20]/@id",[<<"sum">>]},
                {"string-length(name(/html)) = 4",true},
                {"//a[my_fun(@href) > 0]/text()", 
                    [<<"ssddd">>,<<"myURLValue">>]},
                {"/html/body/div[1]/a[1]",
                    [{<<"a">>,[{<<"href">>,<<"sss">>}],[<<"ssddd">>]}]}
                ]}
    ].

test1() ->
	{ok,DocBin} = file:read_file("html-docs/test3.html"),
    Doc = mochiweb_html:parse(DocBin),
    %io:format("~p~n",[Doc]),
	X = mochiweb_xpath:execute("//div[@id='searchTemplate']",Doc),
	%io:format("~n################~nresult: ~p~n", [X]),
	io:format("result len: ~p~n", [length(X)]),
	ok.
	
test3() ->
	mochiweb_http:start([
		{loop, fun(Req) -> io:format("hey~n") end}, 
		{ip, "127.0.0.1"}, 
		{port, 4056}]).	

stupid(Req) ->
	io:format("header: ~p~n", [Req:get(headers)]).
	
test2() ->
	Url = "http://www.amazon.com/s/qid=1232337093/ref=sr_pg_2/182-6187393-2698830?ie=UTF8&rs=14545&rh=n%3A75%2Cn%3A14545&page=2",
	inets:start(),
	{ok, {_,_,Body}} = http:request(get, {Url, []}, [], []),
	ok = file:write_file("html-docs/test3.html", Body),
	Doc = mochiweb_html:parse(Body),
	X = mochiweb_xpath:execute("//div[@id='searchTemplate']",Doc),
	io:format("result len: ~p~n", [length(X)]),
	ok.

%% @doc Functional test
test() ->
    Mapping = [{<<"link">>,1},{<<"myUrl">>,2}],
    % F = simple user-defined function.
    %     First argument is the current XPath context,
    %     Second argument is a list containing the real parameters
    F = fun(_Ctx,[String]) ->
            proplists:get_value(String,Mapping,0)
        end,
    % Function "specification" {name,fun(),parameters signature}
    %           (the engine will take care of converting the parameters
    %            to the appropiate type (node_set|string|number|boolean)
    MyFuns = [{my_fun,F,[string]}],

    lists:foreach(fun(Def) -> do_test(Def,MyFuns) end, test_definitions()).


do_test({File,Cases},UserFunctions) ->
    {ok,DocBin} = file:read_file(File),
    Doc = mochiweb_html:parse(DocBin),
    io:format("----------~s------------~n",[File]),
    lists:foreach(fun({Expr,Expected}) ->
                R = mochiweb_xpath:execute(Expr,Doc,UserFunctions),
                assert(Expr,R,Expected)
              end, Cases).

assert(Expr,Result,Expected) ->
   case Result == Expected of
        true -> 
            io:format("pass: ~s~n",[Expr]);
        false -> 
            io:format("*fail*: ~s : ~n* Result:~p Expected:~p~n",
                        [Expr,Result,Expected])
    end.


