%%% @doc Example GWTCLIENT server
%%% @author Gregory Haskins
%%% @version 1.0
-module(example_gwtclient_server).
-vsn("1.0").

-export([sample_function/0]).

sample_function() ->
    io:fwrite("~p~n", [?MODULE]),
    ok.

sample_private_function() ->
	private.
