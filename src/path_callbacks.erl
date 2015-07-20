-module(path_callbacks).

-export([add/2]).
-export([run/1]).

-include("path.hrl").


-spec add([form_name()], callback()) -> ok.
add(Forms, Callback) ->
    true = ets:insert(get_table(), [{Form, Callback} || Form <- Forms]),
    ok.

-spec run(form()) -> action().
run(Form) ->
    run_callbacks(Form, get_callbacks(Form)).

run_callbacks(Form, []) ->
    {continue, Form};
run_callbacks(Form, [Callback | Rest]) ->
    try
        case Callback(Form) of
            continue ->
                run_callbacks(Form, Rest);
            {continue, NewForm} ->
                run_callbacks(NewForm, Rest);
            stop ->
                {stop, Form};
            {stop, NewForm} ->
                {stop, NewForm}
        end
    catch
        error:function_clause ->
            run_callbacks(Form, Rest)
    end.

get_callbacks(Form) ->
    [C || {_, C} <- ets:lookup(get_table(), element(1, Form))].

get_table() ->
    case get(callbacks) of
        undefined ->
            Table = ets:new(callbacks, [duplicate_bag]),
            _ = put(callbacks, Table),
            Table;
        Table ->
            Table
    end.
