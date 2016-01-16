-module(zole_ws_handler).
-define(PROMPT_REPEAT, 10000).
-define(CONN_TIMEOUT, 200000).
-define(PROMPT_TIMEOUT, 6).
-import(lists,[delete/2,nth/2,split/2,sort/2,map/2,nth/2,zip/2,member/2,reverse/1,foreach/2,all/2]).
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
    {ok, Req, {}, ?CONN_TIMEOUT}.

websocket_handle({text, Msg}, Req, State) ->
    J = jsx:is_json(Msg),
    {Reply, NewState} = if J -> handle(jsx:decode(Msg), State);
			   true -> {{error, invalid_json}, State}
			end,
    {reply, {text, jsx:encode(transform(Reply))}, Req, NewState};
websocket_handle(Data, Req, State) ->
    lager:info("WS handle ~p ~p ~n",[ Data, State]),
    {ok, Req, State}.

websocket_info({timer, Msg, Ref}, Req, {joined, Pid, true, N, Ref} = S) ->
    lager:info("WS timer ~p ~p ~n",[Msg, N]),
    timer:send_after(?PROMPT_REPEAT, {timer, Msg, Ref}),
    if N > ?PROMPT_TIMEOUT ->
	    {shutdown, Req, S};
       true ->
	    {reply, {text, jsx:encode(transform(Msg))}, Req, {joined, Pid, true, N + 1, Ref}}
    end;
websocket_info({timer, _, _}, Req, S) ->
    {ok, Req, S};
websocket_info({table_closed, _ , _} = Msg, Req, _State) ->
    {reply, {text, jsx:encode(transform(Msg))}, Req, {logged_in}};
websocket_info({end_of_game, GameNum, _, _, _, {Score, Points, TotalPoints}}, Req, State) ->
    TricksWon = maps:map(fun( _,{T,_P}) -> T end, Score),
    PointsWon = maps:map(fun( _,{_T,P}) -> P end, Score),
    {reply, {text, jsx:encode(transform({end_of_game, GameNum, TricksWon, PointsWon, Points, TotalPoints}))}, Req, State};
websocket_info(Msg, Req, State) ->
    lager:info("WS info ~p ~n",[Msg]),
    {reply, {text, jsx:encode(transform(Msg))}, Req, waiting_state(Msg, State)}.

websocket_terminate(Reason, _Req, {logged_in} = State) ->
    R = admin:logout(),
    lager:info("WS TERMINATE ~p ~p ~p~n",[Reason, State, R]),
    ok;
websocket_terminate(Reason, _Req, {joined, Pid, _, _, _} = State) ->
    lager:info("WS TERMINATE ~p ~p~n",[Reason, State]),
    table_sup:disconnect(Pid),
    admin:logout(),
    ok;
websocket_terminate(Reason, _Req, State) ->
    lager:info("WS TERMINATE ~p ~p~n",[Reason, State]),
    ok.

waiting_state({prompt, _} = Msg, {joined, Pid, _, _, _}) ->
    Ref = make_ref(),
    timer:send_after(?PROMPT_REPEAT, {timer, Msg, Ref}),
    {joined, Pid, true, 0, Ref};
waiting_state(_, S) ->
    S.

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
    {Resp, NewState} = case R of
		   {ok, Pid} -> {{ok}, {joined, Pid, false, 0, empty}};
		   E -> {E, S}
	       end,
    {response(Resp, join), NewState};
handle([<<"leave">>], {joined, TablePid, _, _, _} = S) ->
    R = table_sup:leave(TablePid),
    NewState = case R of
		   {ok} -> {logged_in};
		   _ -> S
	       end,
    {response(R, leave), NewState};
handle([<<"lielais">>], {joined, TablePid, _, _, _} = S) ->
    R = table_sup:lielais(TablePid),
    {response(R, lielais), new_state(R, S)};
handle([<<"zole">>], {joined, TablePid, _, _, _} = S) ->
    R = table_sup:zole(TablePid),
    {response(R, zole), new_state(R, S)};
handle([<<"pass">>], {joined, TablePid, _, _, _} = S) ->
    R = table_sup:pass(TablePid),
    {response(R, pass), new_state(R, S)};
handle([<<"last_game">>], {joined, TablePid, _, _, _} = S) ->
    R = table_sup:last_game(TablePid),
    {response(R, last_game), S};
handle([<<"tables">>], S) ->
    spawn(?MODULE, get_tables, [self()]),
    {response({ok}, tables), S};
handle([<<"play">>, Crd], {joined, TablePid, _, _, _} = S) ->
    Cs = (catch decode_card(Crd)),
    case Cs of
	{'EXIT', _} ->
	    {response({error, illegal_card}, play), S};
	Card ->
	    lager:info("Playing ~p~n",[Card]),
	    R = table_sup:play(TablePid, Card),
	    {response(R, play), new_state(R, S)}
    end;
handle([<<"save">>, Cds], {joined, TablePid, _, _, _} = S) ->
    Cs = (catch lists:map(fun decode_card/1, Cds)),
    case Cs of
	{'EXIT', _} ->
	    lager:error("Illegal card ~p~n", Cds),
	    {response({error, illegal_card}, save), S};
	Cards ->
	    lager:info("Save ~p~n", [Cards]),
	    R = table_sup:save(TablePid, Cards),
	    {response(R, save), new_state(R, S)}
    end;
handle(_, S) ->
    {{error, illegal_state}, S}.

new_state({ok}, {joined, TablePid, _, _, _}) ->
    {joined, TablePid, false, 0, empty};
new_state(_, S) ->
    S.

get_tables(Pid) ->
    {ok, Tables} = admin:list_avail_tables(),
    Msg = case maps:size(Tables) of
	      0 -> {tables};
	      _ -> {tables, Tables}
	  end,
    Pid ! Msg.

decode_card(Binary) when is_binary(Binary) ->
    list_to_atom(binary_to_list(Binary));
decode_card([N, S]) when is_integer(N) ->
    {N, decode_card(S)};
decode_card([R, S]) ->
    {decode_card(R), decode_card(S)}.

response({ok}, Return) ->
    {ok, Return};
response({error, Msg}, Return) ->
    {error, Msg, Return}.

transform(Tuple) when is_tuple(Tuple) ->
    transform(tuple_to_list(Tuple));
transform(Map) when is_map(Map) ->
    transform(to_list(Map));
transform(List) when is_list(List) ->
    case all(fun erlang:is_integer/1, List) of
	false -> map(fun zole_ws_handler:transform/1, List);
	true -> list_to_binary(List)
    end;
transform(X) ->
    X.
