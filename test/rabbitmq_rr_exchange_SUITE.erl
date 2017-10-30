%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2017 AWeber Communications
%% @end
%%==============================================================================

-module(rabbitmq_rr_exchange_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

all() ->
  [
    {group, non_parallel_tests}
  ].

groups() ->
  [
    {non_parallel_tests, [], [
      position_tests
    ]}
  ].

%% -------------------------------------------------------------------
%% Test suite setup/teardown
%% -------------------------------------------------------------------

init_per_suite(Config) ->
  rabbit_ct_helpers:log_environment(),
  rabbit_ct_helpers:run_setup_steps(Config, rabbit_ct_broker_helpers:setup_steps()).

end_per_suite(Config) ->
  rabbit_ct_helpers:run_teardown_steps(Config, rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(_, Config) ->
  Config.

end_per_group(_, Config) ->
  Config.

init_per_testcase(Testcase, Config) ->
  rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
  rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -------------------------------------------------------------------
%% Test cases
%% -------------------------------------------------------------------

position_tests(_Config) ->
  Values = [#binding{destination={resource,<<"/">>,queue,<<"queue1">>}},
            #binding{destination={resource,<<"/">>,queue,<<"queue2">>}},
            #binding{destination={resource,<<"/">>,queue,<<"queue3">>}},
            #binding{destination={resource,<<"/">>,queue,<<"queue4">>}}],
  4 = rabbitmq_rr_exchange_worker:position({resource,<<"/">>,queue,<<"queue4">>}, Values),
  2 = rabbitmq_rr_exchange_worker:position({resource,<<"/">>,queue,<<"queue2">>}, Values),
  3 = rabbitmq_rr_exchange_worker:position({resource,<<"/">>,queue,<<"queue3">>}, Values),
  1 = rabbitmq_rr_exchange_worker:position({resource,<<"/">>,queue,<<"queue1">>}, Values),
  not_found = rabbitmq_rr_exchange_worker:position({resource,<<"/">>,queue,<<"queue5">>}, Values),
  passed.
