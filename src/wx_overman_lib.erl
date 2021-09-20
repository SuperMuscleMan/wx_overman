%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 9月 2021 14:54
%%%-------------------------------------------------------------------
-module(wx_overman_lib).
%%%=======================STATEMENT====================
-description("wx_overman_lib").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
%%%=======================EXPORT=======================
-export([read_token/1, ls/1, ls/2, is_variant/1]).
%%%=======================INCLUDE======================
-include_lib("kernel/include/file.hrl").
%%%=======================RECORD=======================

%%%=======================DEFINE=======================

%%%=================EXPORTED FUNCTIONS=================

%% -----------------------------------------------------------------
%% Func:
%% Description:是否变量
%% Returns:
%% -----------------------------------------------------------------
is_variant([H | _]) ->
	is_variant_(H);
is_variant(V) when is_atom(V) ->
	[H | _] = atom_to_list(V),
	is_variant_(H);
is_variant(_) ->
	false.
is_variant_(H) ->
	H >= $A andalso H =< $Z.


%% -----------------------------------------------------------------
%% Func:
%% Description:从内容为erlang term的文件中读取token
%% Returns:
%% -----------------------------------------------------------------
read_token(FilePath) ->
	case file:read_file(FilePath) of
		{ok, Bin} ->
			Body = unicode:characters_to_list(Bin, utf8),
			erl_scan:string(Body);
		Err ->
			Err
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description: 遍历指定深度的目录
%% Returns: [{Path, {FileName, Extension, Size, Mtime, []}}]
%% -----------------------------------------------------------------
ls(Path) ->
	ls(Path, 1).
ls(Path, Depth) when is_list(Path); is_binary(Path) ->
	case file:list_dir(Path) of
		{ok, Files} ->
			ls_(Files, Path, Depth, []);
		Err ->
			Err
	end.
ls_([], _Path, _Depth, Result) ->
	Result;
ls_(_, _Path, 0, Result) ->
	Result;
ls_([H | T], Path, Depth, Result) ->
	SubPath = filename:append(Path, H),
	case file:read_file_info(SubPath) of
 		{ok, #file_info{type = directory}} ->
			case file:list_dir(SubPath) of
				{ok, Files} ->
					Result1 = ls_(Files, SubPath, Depth - 1, Result),
					ls_(T, Path, Depth, Result1);
				Err ->
					{SubPath, Err}
			end;
		{ok, #file_info{type = regular, size = S, mtime = Mt}} ->
			Extension = filename:extension(H),
			ls_(T, Path, Depth, [{Path, {H, Extension, S, Mt, []}} | Result]);
		_ ->
			ls_(T, Path, Depth, Result)
	end.
%==========================DEFINE=======================