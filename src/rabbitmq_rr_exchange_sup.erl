%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2017 AWeber Communications
%% @end
%%==============================================================================

%% @doc Supervisor for the Round-Robin Exchange worker
%% @end

-module(rabbitmq_rr_exchange_sup).

-behaviour(supervisor).

-export([init/1, start_link/0, stop/0]).

-rabbit_boot_step({rabbitmq_rr_exchange_sup,
                   [{description, "Round-Robin Exchange Worker Supervisor"},
                    {mfa,         {rabbit_sup, start_child, [?MODULE]}},
                    {requires,    rabbit_registry},
                    {cleanup,     {?MODULE, stop, []}},
                    {enables,     recovery}]}).

-include_lib("rabbit_common/include/rabbit.hrl").

start_link() ->
  supervisor2:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 3, 10},
    [{rabbitmq_rr_exchange_worker,
      {rabbitmq_rr_exchange_worker, start_link, []},
      permanent,
      10000,
      worker,
      [rabbitmq_rr_exchange_worker]}
    ]}}.


stop() ->
  ok = supervisor:terminate_child(rabbit_sup, ?MODULE),
  ok = supervisor:delete_child(rabbit_sup, ?MODULE).
