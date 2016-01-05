-module(zole_ws_handler).
-import(lists,[delete/2,nth/2,split/2,sort/2,map/2,nth/2,zip/2,member/2,reverse/1,foreach/2]).
-import(maps,[get/2,put/3,from_list/1,to_list/1,is_key/2,keys/1,get/3,update/3]).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-compile(export_all).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
        lager:info("WS INIT ~n",[]),
%	erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, Req, {}}.

websocket_handle({text, Msg}, Req, State) ->
    J = jsx:is_json(Msg),
    {Reply, NewState} = if J -> handle(jsx:decode(Msg), State);
			   true -> {{error, invalid_json}, State}
			end,
    {reply, {text, jsx:encode(transform(Reply))}, Req, NewState};
websocket_handle(Data, Req, State) ->
        io:format("WS handle ~p ~p ~n",[ Data, State]),
	{ok, Req, State}.

websocket_info(Msg, Req, State) ->
        lager:info("WS info ~p ~n",[Msg]),
%	erlang:start_timer(1000, self(), <<"How' you doin'?", State/binary>>),
%	{reply, {text, Msg}, Req, State};
    {reply, {text, jsx:encode(transform(Msg))}, Req, State}.

websocket_terminate(Reason, _Req, State) ->
        io:format("WS TERMINATE ~p ~p~n",[Reason, State]),
	ok.

handle([<<"login">>, N], {} = S) ->
    Name = binary_to_list(N),
    R = admin:login(Name),
    NewState = case R of
		   {ok} -> {logged_in};
		   _ -> S
	       end,
    {response(R, login), NewState};
handle([<<"logout">>], {logged_in} = S) ->
    R = admin:logout(),
    NewState = case R of
		   {ok} -> {};
		   _ -> S
	       end,
    {response(R, logout), NewState};
handle([<<"join">>, N], {logged_in} = S) ->
    TableName = binary_to_list(N),
    R = table_sup:join_or_create(TableName, true),
    NewState = case R of
		   {ok, Pid} -> {joined, Pid};
		   _ -> S
	       end,
    {response(R, join), NewState};
handle([<<"leave">>], {joined, TablePid} = S) ->
    R = table_sup:leave(TablePid),
    NewState = case R of
		   {ok} -> {logged_in};
		   _ -> S
	       end,
    {response(R, leave), NewState};
handle([<<"lielais">>], {joined, TablePid} = S) ->
    R = table_sup:lielais(TablePid),
    {response(R, lielais), S};
handle([<<"zole">>], {joined, TablePid} = S) ->
    R = table_sup:zole(TablePid),
    {response(R, zole), S};
handle([<<"pass">>], {joined, TablePid} = S) ->
    R = table_sup:pass(TablePid),
    {response(R, pass), S};
handle([<<"last_game">>], {joined, TablePid} = S) ->
    R = table_sup:last_game(TablePid),
    {response(R, last_game), S};
handle([<<"play">>, Card], {joined, TablePid} = S) ->
    Cs = (catch decode_card(Card)),
    case Cs of
	{'EXIT', _} ->
	    {response({error, illegal_card}, save), S};
	Card ->
	    R = table_sup:play(Card, TablePid),
	    {response(R, play), S}
    end;
handle([<<"save">>, Cds], {joined, TablePid} = S) ->
    Cs = (catch lists:map(fun decode_card/1, Cds)),
    case Cs of
	{'EXIT', _} ->
	    {response({error, illegal_card}, save), S};
	Cards ->
	    R = table_sup:save(Cards, TablePid),
	    {response(R, save), S}
    end;
handle(_, S) ->
    {{error, illegal_state}, S}.

decode_card(Binary) when is_binary(Binary) ->
    list_to_existing_atom(binary_to_list(Binary));
decode_card([R,<<"7">>]) ->
    {decode_card(R), 7};
decode_card([R,<<"8">>]) ->
    {decode_card(R), 8};
decode_card([R,<<"9">>]) ->
    {decode_card(R), 9};
decode_card([R,<<"10">>]) ->
    {decode_card(R), 10};
decode_card([R, S]) ->
    {decode_card(R), decode_card(S)}.

response({ok}, Return) ->
    {ok, Return};
response({error, Msg}, Return) ->
    {error, Msg, Return}.

transform(Tuple) when is_tuple(Tuple) ->
    lists:map(fun zole_ws_handler:transform/1, tuple_to_list(Tuple));
transform(Map) when is_map(Map) ->
    transform(maps:to_list(Map));
transform(X) ->
    X.
