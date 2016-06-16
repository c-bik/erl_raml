-module(erl_raml).

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(yamerl),
    application:start(jsx),
    application:start(?MODULE).

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    application:stop(?MODULE),
    application:stop(jsx),
    application:stop(yamerl).

stop(_State) -> ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) -> {ok, { {one_for_one, 5, 10}, []} }.
