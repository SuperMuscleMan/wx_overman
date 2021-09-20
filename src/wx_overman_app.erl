%%%-------------------------------------------------------------------
%% @doc wx_overman public API
%% @end
%%%-------------------------------------------------------------------

-module(wx_overman_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    wx_overman_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
