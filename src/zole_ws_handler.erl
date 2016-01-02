-module(zole_ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
        io:format("WS INIT ~n",[]),
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, Req, <<"undefined">>}.

websocket_handle({text, Msg}, Req, State) ->

    J  = jsx:is_json(Msg),

        io:format("WS handle ~p ~p ~p~n",[Msg, State, J]),
    %{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State},
    T = jsx:encode([cards,[[{a,b}],[{c,d}]]]),

    Ns = if J -> jsx:decode(Msg);
       true -> State
    end,

    {reply, {text, T}, Req, Ns};
websocket_handle(Data, Req, State) ->
        io:format("WS handle ~p ~p ~n",[ Data, State]),
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
        io:format("WS info ~p ~p~n",[Msg, State]),
	erlang:start_timer(1000, self(), <<"How' you doin'?", State/binary>>),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
        io:format("WS handle ~p ~p ~n",[ Req, State]),
	{ok, Req, State}.

websocket_terminate(Reason, _Req, State) ->
        io:format("WS TERMINATE ~p ~p~n",[Reason, State]),
	ok.
