-module(timecheck).
%% API
-export([tc/0]).
-compile(export_all).

tc() ->
    Sqerl = {select,'*',{from,{foo,join,bar,[
        {'and', [
            {'foo.bar_a','=','$a'},
            {'foo.bar_b','=','$b'},
            {'foo.bar_d','=','$d'},
            {'foo.bar_f','=','$f'},
            {'foo.bar_c','=','$c'},
            {'foo.bar_g','=','$g'},
            {'foo.bar_h','=','$h'},
            {'foo.bar_e','=','$e'},
            {'foo.bar_i','=','$i'},
            {'foo.bar_j','=','$j'}
        ]
        }]}}},
    tc(10, Sqerl),
    ok.

tc(0, _) ->
    ok;
tc(I, Sqerl) ->
    BindList = [{Chr, rand:uniform(100)} || Chr <- [a, b, c, d, e, f, g, h, i, j]],
    Binds = maps:from_list(BindList),
    {T1, _} = timer:tc(fun() -> sqerl:sql(Sqerl, true) end),
    {T2, _} = timer:tc(fun() -> sqerl:sqlb(Sqerl, Binds, true) end),
    io:format("~p ~p~n", [T1, T2]),
    tc(I - 1, Sqerl).
