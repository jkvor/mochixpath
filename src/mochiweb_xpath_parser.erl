%% Copyright (c) 2008-2009 
%% Pablo Polvorin <ppolv@jabberes.org> [http://ppolv.wordpress.com]
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%%% @doc Compile XPath expressions.
%% This module uses the xpath parser of xmerl.. that interface isn't documented
%% so could change between OTP versions.. its know to work with OTP R12B2 
%% created on 2008-05-07
-module(mochiweb_xpath_parser).

-export([compile_xpath/1]).

%% Return a compiled XPath expression
compile_xpath(XPathString) ->
    {ok,XPath} = xmerl_xpath_parse:parse(xmerl_xpath_scan:tokens(XPathString)),
    simplify(XPath).



%% @doc Utility functions to convert between the *internal* representation of
%       xpath expressions used in xmerl(using lists and atoms), to a
%       representation using only binaries, to match the way in
%       which the mochiweb html parser represents data 
simplify({path,Type,Path}) ->
    {path,Type,simplify_path(Path)};
simplify({comp,Comp,A,B}) ->
    {comp,Comp,simplify(A),simplify(B)};
simplify({literal,L}) ->
    {literal,list_to_binary(L)};
simplify({number,N}) ->
    {number,N};
simplify({function_call,Fun,Args}) ->
    {function_call,Fun,lists:map(fun simplify/1,Args)}.

simplify_path({step,{Axis,NodeTest,Predicates}}) ->
    {step,{Axis,
            simplify_node_test(NodeTest),
            simplify_predicates(Predicates)}};
simplify_path({refine,Path1,Path2}) ->
    {refine,simplify_path(Path1),simplify_path(Path2)}.

simplify_node_test({name,{Tag,Prefix,Local}}) ->
    {name,{to_binary(Tag),Prefix,Local}};

simplify_node_test(A={node_type,Type}) when Type == 'text' ;
                                            Type == 'node' ->
    A;
simplify_node_test(A={wildcard,wildcard}) ->
    A.

simplify_predicates(X) -> lists:map(fun simplify_predicate/1,X).
simplify_predicate({pred,Pred}) ->
    {pred,simplify(Pred)}.

to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X)).




