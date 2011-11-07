-module(trim).
-author('Steve Davis < steven · charles · davis ? gmail · com >').
-export([trim/1]).

trim(Bin) when is_binary(Bin) ->
    list_to_binary(trim(binary_to_list(Bin)));
trim(String) when is_list(String) ->
    String2 = lists:dropwhile(fun is_whitespace/1, String),
    lists:reverse(lists:dropwhile(fun is_whitespace/1, lists:reverse(String2))).

is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_Else) -> false.
