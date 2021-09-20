-module(wx_time).
%%%=======================STATEMENT====================
-description("wx_time").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
%%%=======================EXPORT=======================
-export([now_second/0, now_millisec/0, second_to_datetime/1, date_to_second/1, second_to_local/1,
	local_second/0, millisecond_to_datetime/1, local_millisec/0, second_to_local_datetime/1,
	get_time_zone/0]).
%%%=======================INCLUDE======================

%%%=======================RECORD=======================

%%%=======================DEFINE=======================
-define(Cfg, '$$time').
-define(Zone_key, zone).
%%%=================EXPORTED FUNCTIONS=================

%% -----------------------------------------------------------------
%% Func:
%% Description:获取本地时区 *（无需在业务逻辑中获取，仅用于初始化数据使用
%% Returns:
%% -----------------------------------------------------------------
get_time_zone() ->
	case wx_cfg:get(?Cfg, ?Zone_key) of
		none ->
			get_time_zone_();
		V ->
			V
	end.
get_time_zone_() ->
	Sec = now_second(),
	Zone =
		erlang:universaltime_to_posixtime(
			erlang:universaltime_to_localtime(
				erlang:posixtime_to_universaltime(Sec)))
			- Sec,
	wx_cfg:set(?Cfg, ?Zone_key, Zone),
	Zone.
%% -----------------------------------------------------------------
%% Func:
%% Description:获取当前时间戳 UTC 0
%% Returns:
%% -----------------------------------------------------------------
now_second() ->
	erlang:system_time(second).

%% -----------------------------------------------------------------
%% Func:
%% Description:获取当前时间戳(毫秒) UTC 0
%% Returns:
%% -----------------------------------------------------------------
now_millisec() ->
	erlang:system_time(millisecond).
%% -----------------------------------------------------------------
%% Func:
%% Description:获取本地时间戳
%% Returns:
%% -----------------------------------------------------------------
local_second() ->
	second_to_local(now_second()).

%% -----------------------------------------------------------------
%% Func:
%% Description:获取本地毫秒时间戳
%% Returns:
%% -----------------------------------------------------------------
local_millisec() ->
	millisecond_to_local(now_millisec()).

%% -----------------------------------------------------------------
%% Func:
%% Description:时间戳转日期时间
%% Returns:
%% -----------------------------------------------------------------
second_to_datetime(Sec) ->
	erlang:posixtime_to_universaltime(Sec).

%% -----------------------------------------------------------------
%% Func:
%% Description:时间戳转本地日期时间
%% Returns:
%% -----------------------------------------------------------------
second_to_local_datetime(Sec) ->
	second_to_datetime(second_to_local(Sec)).
%% -----------------------------------------------------------------
%% Func:
%% Description:毫秒时间戳转日期时间
%% Returns:
%% -----------------------------------------------------------------
millisecond_to_datetime(MilliSec) ->
	Sec = MilliSec div 1000,
	{Date, Time} = second_to_datetime(Sec),
	Milli = MilliSec rem 1000,
	{Date, Time, Milli}.

%% -----------------------------------------------------------------
%% Func:
%% Description:时间戳转本地时间戳
%% Returns:
%% -----------------------------------------------------------------
second_to_local(Sec) ->
	Sec + get_time_zone().

%% -----------------------------------------------------------------
%% Func:
%% Description:时间戳转本地时间戳
%% Returns:
%% -----------------------------------------------------------------
millisecond_to_local(Sec) ->
	Sec + get_time_zone() * 1000.


%% -----------------------------------------------------------------
%% Func:
%% Description:日期时间转时间戳
%% Returns:
%% -----------------------------------------------------------------
date_to_second(Data) ->
	erlang:universaltime_to_posixtime(Data).
%==========================DEFINE=======================