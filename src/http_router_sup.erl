-module(http_router_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, ConfigPath} = application:get_env(http_router, config_path),
  {ok, Frequency} = application:get_env(http_router, frequency),
  http_router_compiler:check(ConfigPath),
  if
    is_number(Frequency) -> timer:apply_interval(Frequency, http_router_compiler, check, [ConfigPath]);
    true -> ok
  end,
  {ok, { {one_for_one, 5, 10}, []} }.

