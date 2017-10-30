%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2017 AWeber Communications
%% @end
%%==============================================================================

%% @doc gen_server process for listening to casts and calls from rabbitmq_rr_exchange
%% @end

-module(rabbitmq_rr_exchange_worker).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("rabbit_common/include/rabbit.hrl").

-record(state, {xs}).

% -------------------------
% Worker Startup
% -------------------------

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
  rabbit_log:info("Starting round-robin exchange worker."),
  {ok, #state{xs=dict:new()}}.

% -------------------------
% Gen Server Implementation
% -------------------------

handle_call({create, Name}, _From, State) ->
  {reply, ok, State#state{xs=dict:store(Name, nil, State#state.xs)}};

handle_call({delete, Name}, _From, State) ->
  {reply, ok, State#state{xs=dict:erase(Name, State#state.xs)}};

handle_call({route, Name}, _From, State) ->
  case get_destination(Name, last_value(Name, State)) of
    {ok, Destination} ->
      {reply, {ok, [Destination]}, #state{xs=dict:store(Name, Destination, State#state.xs)}};
    error ->
      {reply, {ok, []}, #state{xs=dict:store(Name, nil, State#state.xs)}}
  end;

handle_call(_Msg, _From, State) ->
  {noreply, unknown_command, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_, State, _) ->
  {ok, State}.

terminate(_,_) ->
  rabbit_log:info("Terminating round-robin exchange worker."),
  ok.

% -------------------------
% Internal Methods
% -------------------------

-spec last_value(rabbit_exchange:name(), #state{}) -> {ok, rabbit_types:binding_destination()} | nil.
last_value(Name, State) ->
  return_last_value(dict:find(Name, State#state.xs)).

-spec return_last_value({ok, rabbit_types:binding_destination()} | nil) -> rabbit_types:binding_destination() | nil.
return_last_value(error) -> nil;
return_last_value({ok, Value}) -> Value.

-spec get_destination(rabbit_exchange:name(), rabbit_types:binding_destination() | nil) ->
  {ok, rabbit_types:binding_destination()} | error.
get_destination(Name, Last) ->
  Bindings = rabbit_binding:list_for_source(Name),
  case get_binding(Last, Bindings) of
    {ok, Binding} -> {ok, Binding#binding.destination};
    error -> error
  end.

-spec get_binding(rabbit_types:binding_destination(), list()) -> {ok, rabbit_types:binding()} | error.
get_binding(_, []) -> error;
get_binding(Last, Bindings) ->
  Index = case position(Last, Bindings) of
            not_found -> 0;
            Value -> Value
          end,
  Offset = case (Index + 1) > length(Bindings) of
             false -> Index + 1;
             true -> 1
           end,
  {ok, lists:nth(Offset, Bindings)}.

-spec position(rabbit_types:binding_destination(), list()) -> integer().
position(Value, List) ->
  position(Value, List, 1).

-spec position(rabbit_types:binding_destination(), list(), integer()) -> integer().
position(nil, _, _) -> not_found;
position(_, [], _)  -> not_found;
position(Value, [#binding{destination=Dest}|_], Index) when Dest =:= Value ->
  Index;
position(Value, [_|T], Index) ->
  position(Value, T, Index + 1).
