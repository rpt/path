-module(path).

-export([add_callback/2]).
-export([walk/1]).

-include("path.hrl").


-spec add_callback(form_name() | [form_name()], callback()) -> ok.
add_callback(Forms, Callback) when is_list(Forms) ->
    path_callbacks:add(Forms, Callback);
add_callback(Form, Callback) ->
    path_callbacks:add([Form], Callback).

-spec walk(ast()) -> ast().
walk(Forms) ->
    forms(Forms).

forms(Forms) ->
    [callback(Form) || Form <- Forms].

form({call, Line, Call, Args}) ->
    {call, Line, callback(Call), forms(Args)};
form({clause, Line, Patterns, Guard, Body}) ->
    {clause, Line, forms(Patterns), Guard, forms(Body)};
form({clauses, Clauses}) ->
    {clauses, forms(Clauses)};
form({'fun', Line, Fun}) ->
    {'fun', Line, callback(Fun)};
form({function, Module, Function, Arity}) ->
    {function, callback(Module), callback(Function), callback(Arity)};
form({function, Line, Function, Arity, Clauses}) ->
    {function, Line, Function, Arity, forms(Clauses)};
form({map, Line, Fields}) ->
    {map, Line, forms(Fields)};
form({map, Line, Var, Fields}) ->
    {map, Line, form(Var), forms(Fields)};
form({map_field_assoc, Line, Key, Value}) ->
    {map_field_assoc, Line, callback(Key), callback(Value)};
form({map_field_exact, Line, Key, Value}) ->
    {map_field_exact, Line, callback(Key), callback(Value)};
form({match, Line, Left, Right}) ->
    {match, Line, callback(Left), callback(Right)};
form({named_fun, Line, Name, Clauses}) ->
    {named_fun, Line, Name, forms(Clauses)};
form({remote, Line, Module, Function}) ->
    {remote, Line, callback(Module), callback(Function)};
form(SimpleForm) ->
    _ = simple(SimpleForm),
    SimpleForm.

simple({atom, _, _}) -> ok;
simple({attribute, _, _, _}) -> ok;
simple({eof, _}) -> ok;
simple({float, _, _}) -> ok;
simple({function, _, _}) -> ok; %% `fun foo/1`
simple({integer, _, _}) -> ok;
simple({string, _, _}) -> ok;
simple({var, _, _}) -> ok;
simple(UnknownForm) ->
    io:format("Unknown form: ~p~n", [UnknownForm]).

callback(Form) ->
    case path_callbacks:run(Form) of
        {continue, NewForm} ->
            form(NewForm);
        {stop, NewForm} ->
            NewForm
    end.
