-module(path_utils).

-export([spy/1]).
-export([spy_walk/1]).

-include("path.hrl").


-spec spy(term()) -> term().
spy(X) ->
    _ = io:format("~p~n", [X]),
    X.

-spec spy_walk(ast()) -> ast().
spy_walk(Forms) ->
    spy(path:walk(spy(Forms))).
