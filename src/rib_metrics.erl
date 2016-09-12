-module(rib_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0,
         report/0,
         observe/3,
         inc/2]).

%% gen_server API
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).

%% API

start_link() ->
    {ok, _} = application:ensure_all_started(prometheus),
    ok = declare_metrics(),
    gen_server:start_link({local, rib_metrics}, ?MODULE, {}, []).

inc(Metric, Labels) ->
    gen_server:cast(rib_metrics, {inc, Metric, Labels}).

observe(Metric, Labels, Value) ->
    gen_server:cast(rib_metrics, {observe, Metric, Labels, Value}).

report() ->
    prometheus_text_format:format().

%% gen_server API

init({}) ->
    {ok, {}}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

handle_call(_Req, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(Message, State) ->
    ok = handle_message(Message),
    {noreply, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%% Implementation

handle_message({inc, Metric, Labels}) ->
    prometheus_counter:inc(Metric, Labels);
handle_message({observe, Metric, Labels, Value}) ->
    prometheus_histogram:observe(Metric, Labels, round(Value));
handle_message(M) ->
    ok = error_logger:info_report({?MODULE, M}),
    ok.

declare_metrics() ->
    prometheus_counter:declare(
      [{name, rib_fetch_response},
       {labels, [status]},
       {help, "backend http response counter"}]),
    prometheus_histogram:declare(
      [{name, rib_callback_time_taken_msec},
       {buckets, msec_buckets()},
       {labels, [status]},
       {help, "frontend http response elapsed time in microsec"}]),
    ok.

msec_buckets() ->
    Secs = [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5],
    [Sec * 1.0e3 || Sec <- Secs].
