%%% @author Gavin M. Roy <gavinmroy@gmail.com>
%%% @copyright 2015, Gavin M. Roy
%%% @doc Mnesia clustering made easy
%%% @version 0.1.0
-module(emnesia).

-behavior(gen_server).

-export([start/0, start/2, stop/0]).

-export([start_link/0,
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

%% @doc Start the application
-spec start() -> {ok, [atom()]}.
start() ->
    {ok, _} = application:ensure_all_started(emnesia).

%% @doc Start the application
-spec start(atom(), term()) -> {ok, [atom()]}.
start(_Type, _Args) ->
  emnesia_sup:start_link().

%% @doc Stop the application
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(emnesia).

%% @private
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
-spec init([term()]) -> term().
init([]) ->
  {ok, {}}.

%% @private
handle_call(Request, From, State) ->
  io:format("Unknown call ~p from ~p", [Request, From]),
  {noreply, State}.

%% @private
handle_cast(Request, State) ->
  io:format("Unknown cast ~p", [Request]),
  {noreply, State}.

%% @private
handle_info(Info, State) ->
  io:format("Unknown info ~p", [Info]),
  {noreply, State}.

%% @private
terminate(_, _) ->
    ok.

%% @private
code_change(_, _, State) ->
    {ok, State}.
