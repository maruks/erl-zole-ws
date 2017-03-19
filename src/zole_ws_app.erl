%%%-------------------------------------------------------------------
%% @doc zole_ws public API
%% @end
%%%-------------------------------------------------------------------

-module(zole_ws_app).

-behaviour(application).

-export([start/2,stop/1]).

-export([start/0]).

start() ->
    application:ensure_all_started(zole_ws).

start(_StartType, _StartArgs) ->
    {ok, PortNum} = application:get_env(port),
    Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, zole_ws, "index.html"}},
			{"/websocket", zole_ws_handler, []},
			{"/js/[...]", cowboy_static, {priv_dir, zole_ws, "js"}},
			{"/css/[...]", cowboy_static, {priv_dir, zole_ws, "css"}},
			{"/images/[...]", cowboy_static, {priv_dir, zole_ws, "images"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, PortNum}],
		[{env, [{dispatch, Dispatch}]}]),
    lager:info("Started on port ~p~n",[PortNum]),
    zole_ws_sup:start_link().

stop(_State) ->
    ok.
