%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2017 AWeber Communications
%% @end
%%==============================================================================

%% @doc RabbitMQ Round-Robin Exchange Plugin
%% @end

-module(rabbitmq_rr_exchange).

-behaviour(rabbit_exchange_type).

-export([add_binding/3,
         assert_args_equivalence/2,
         create/2,
         description/0,
         delete/3,
         info/1,
         info/2,
         policy_changed/2,
         recover/2,
         route/2,
         remove_bindings/3,
         serialise_events/0,
         validate/1,
         validate_binding/2]).

-include_lib("rabbit_common/include/rabbit.hrl").

-define(TYPE, <<"x-round-robin">>).

-rabbit_boot_step({?MODULE,
  [{description, "exchange type x-round-robin: registry"},
    {mfa,         {rabbit_registry, register, [exchange, ?TYPE, ?MODULE]}},
    {requires,    rabbit_registry},
    {cleanup,     {rabbit_registry, unregister, [exchange, ?TYPE]}},
    {enables,     recovery}]}).

add_binding(none, _, _) -> ok;
add_binding(transaction, _, _) -> ok.

assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).

create(none, _X) -> ok;
create(transaction, #exchange{name=Name}) ->
  gen_server_call({create, Name}).

description() ->
  [{description, <<"Round-Robin Exchange">>}].

delete(none, _, _) -> ok;
delete(transaction, #exchange{name=Name}, _) ->
  gen_server_call({delete, Name}).

info(_X) -> [].
info(_X, _) -> [].

policy_changed(_X1, _X2) -> ok.

recover(_, _) -> ok.

remove_bindings(none, _, _) -> ok;
remove_bindings(transaction, _, _) -> ok.

route(#exchange{name=Name}, _) ->
  case gen_server_call({route, Name}) of
    {ok, Destinations} -> Destinations;
    {error, _} -> []
  end.

serialise_events() ->
  false.

validate(_X) ->
  ok.

validate_binding(_X, _B) ->
  ok.

%% @private
%% @spec get_server_call(Args) -> Reply
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Reply        = ok|{error, Reason}
%% @doc Wrap the gen_server:call behavior to shutdown the channel with an
%%      exception if an error bubbles up from the worker.
%% @end
%%
gen_server_call(Args) ->
  case gen_server:call({global, rabbitmq_rr_exchange_worker}, Args) of
    ok -> ok;
    {ok, Response} ->
      {ok, Response};
    {error, Reason} ->
      rabbit_misc:protocol_error(resource_error, "rabbitmq_rr_exchange_worker failure (~s)", [Reason])
  end.