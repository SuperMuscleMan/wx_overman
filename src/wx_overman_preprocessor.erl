%%%-------------------------------------------------------------------
%%% @author WeiMengHuan
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%	预处理器
%%% 用于检索项目配置，返回child_spec()
%%% @end
%%% Created : 19. 9月 2021 14:44
%%%-------------------------------------------------------------------
-module(wx_overman_preprocessor).
%%%=======================STATEMENT====================
-description("wx_overman_handle").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
%%%=======================EXPORT=======================
-export([start/0]).
%%%=======================INCLUDE======================

%%%=======================RECORD=======================

%%%=======================DEFINE=======================

-define(SERVER, ?MODULE).
-define(FileDepth, 10).  %%文件遍历深度
-define(Extension_Cfg, ".cfg").  %%后缀-配置文件
-define(Extension_Beam, ".beam").  %%后缀-Beam文件
%% 配置变量
-define(Var_Local, var_l).%% 本地
-define(Var_Global, var_g).%% 全局
%% 忽略配置中相同key
-define(IsIgnore_Boot, boot).

-define(CfgType_Boot, boot).%%启动
-define(CfgType_Mf, mf).%%模块配置
-define(CfgType_Cfg, wx_cfg).%%通用配置
-define(CfgType_Init, init).%%初始化
-define(CfgType_Ignore, [var_g, var_l, ?CfgType_Init,
	?CfgType_Mf, ?CfgType_Boot, ?CfgType_Cfg]).
%%%=================EXPORTED FUNCTIONS=================

%% -----------------------------------------------------------------
%% Description:
%% Inputs:
%% Returns: 
%% -----------------------------------------------------------------
start() ->
	search_project().
%==========================DEFINE=======================
%% -----------------------------------------------------------------
%% Description:检索所有项目
%% Inputs:
%% Returns:
%% -----------------------------------------------------------------
search_project() ->
	Dir = get_lib_dir(),
	{ok, List} = file:list_dir(Dir),
	{Boot, Mf, Cfg, Init, Other} =
		parse_project(lists:reverse(List), Dir, [], [], [], [], []),
	run(Boot, Mf, Cfg, Init, Other).
parse_project([Path | T], Dir, Boot, Mf, Cfg, Init, Other) ->
	ListFile = wx_overman_lib:ls(filename:append(Dir, Path), ?FileDepth),
	SortFile = sort_file(ListFile),
	CfgList = parse_cfg(proplists:get_value(?Extension_Cfg, SortFile, [])),
	{Boot1, Mf1, Cfg1, Init1, Other1} =
		parse_project_(CfgList, Boot, Mf, Cfg, Init, Other),
%%	Mf1 = Mf ++ proplists:get_value(mf, CfgList, []),
%%	CfgList1 = lists:keystore(mf, 1, CfgList, {mf, Mf ++ Mf1}),
%%	ok = wx_execute:run(CfgList1),
	parse_project(T, Dir, Boot1, Mf1, Cfg1, Init1, Other1);
parse_project([], _, Boot, Mf, Cfg, Init, Other) ->
	{Boot, Mf, Cfg, Init, Other}.
parse_project_([{?CfgType_Boot, H} | T], [], Mf, Cfg, Init, Other) ->
	parse_project_(T, H, Mf, Cfg, Init, Other);
parse_project_([{?CfgType_Boot, H} | T], Boot, Mf, Cfg, Init, Other) ->
	parse_project_(T, [H | Boot], Mf, Cfg, Init, Other);
parse_project_([{?CfgType_Mf, H} | T], Boot, Mf, Cfg, Init, Other) ->
	parse_project_(T, Boot, init_mf(H, Mf), Cfg, Init, Other);
parse_project_([{?CfgType_Cfg, H} | T], Boot, Mf, Cfg, Init, Other) ->
	parse_project_(T, Boot, Mf, [H | Cfg], Init, Other);
parse_project_([{?CfgType_Init, H} | T], Boot, Mf, Cfg, [], Other) ->
	parse_project_(T, Boot, Mf, Cfg, H, Other);
parse_project_([{?CfgType_Init, H} | T], Boot, Mf, Cfg, Init, Other) ->
	parse_project_(T, Boot, Mf, Cfg, [H | Init], Other);
parse_project_([{?Var_Global, _} | T], Boot, Mf, Cfg, Init, Other) ->
	parse_project_(T, Boot, Mf, Cfg, Init, Other);
parse_project_([{?Var_Local, _} | T], Boot, Mf, Cfg, Init, Other) ->
	parse_project_(T, Boot, Mf, Cfg, Init, Other);
parse_project_([{_, H} | T], Boot, Mf, Cfg, Init, Other) ->
	parse_project_(T, Boot, Mf, Cfg, Init, [H | Other]);
parse_project_([], Boot, Mf, Cfg, Init, Other) ->
	{Boot, Mf, Cfg, Init, Other}.


%% -----------------------------------------------------------------
%% Func:
%% Description:解析配置文件
%% Returns:
%% -----------------------------------------------------------------
parse_cfg(FileList) ->
	Ets = ets:new(parse_cfg, [set, named_table, protected, {keypos, 1}]),
	FileList1 = parse_cfg_(FileList, Ets, []),
	R = parse_token(FileList1, Ets, []),
	ets:delete(Ets),
	R.
parse_cfg_([{Path, {FileName, Extension, Size, Mt, Info}} | T], Ets, Result)
	when Extension == ?Extension_Cfg ->
	DirPath = get_cur_project_path(Path),
	FilePath = filename:append(Path, FileName),
	set_var(Ets, ?Var_Local, 'CurProjectPath', Path, {ok, DirPath}),
	case wx_overman_lib:read_token(FilePath) of
		{ok, Token, _} ->
			token_init_var(Token, Ets, Path, 0, FileName),
			parse_cfg_(T, Ets, [{Path, {FileName, Extension, Size, Mt,
				[{token, {ok, Token}} | Info]}} | Result]);
		Err ->
			erlang:error([{err_parse_cfg, Err}, {file, FilePath}])
	end;
parse_cfg_([], _Ets, Result) ->
	Result.

parse_token([{Path, {FileName, Extension, _Size, _Mt, Info}} | T], Ets, Result)
	when Extension == ?Extension_Cfg ->
	DirPath = get_cur_project_path(Path),
	FilePath = filename:append(Path, FileName),
	set_var(Ets, ?Var_Local, 'CurProjectPath', Path, {ok, DirPath}),
	case proplists:get_value(token, Info, none) of
		{ok, Token} ->
			Token1 = token_trans_var(Token, Ets, Path, 0, []),
			Term = token_to_term(Token1, Ets, FilePath),
			Result1 = sort_cfg(Term, Result),
			parse_token(T, Ets, Result1);
		Err ->
			erlang:error([{err_parse_cfg, Err}, {file, FilePath}])
	end;
parse_token([], _Ets, Result) ->
	Result.
token_init_var([], _, _, _Line, _FileName) ->
	ok;
token_init_var([{'{', _} = H, {'{', _} = H1, {atom, _, VarSign} = H2 | T],
		Ets, FilePath, Line, FileName) when VarSign == var_l;VarSign == var_g ->
	case parse_var(T, 1, [H2, H1, H], []) of
		{{{_, Var}, {_, _, _} = MFA}, T1, _} ->
			check_var_duplicate(Ets, VarSign, Var, FilePath, FileName),
			set_var(Ets, VarSign, Var, FilePath, MFA),
			token_init_var(T1, Ets, FilePath, Line, FileName);
		{{{_, Var}, A}, T1, _} ->
			check_var_duplicate(Ets, VarSign, Var, FilePath, FileName),
			set_var(Ets, VarSign, Var, FilePath, {ok, A}),
			token_init_var(T1, Ets, FilePath, Line, FileName);
		_ ->
			erlang:error([{err_token_trans_var, "invalid var"},
				{file, FilePath}, {line, Line}])
	end;
token_init_var([{'{', _} | T], Ets, FilePath, Line, FileName) ->
	token_init_var(T, Ets, FilePath, Line + 1, FileName);
token_init_var([{'}', _} | T], Ets, FilePath, Line, FileName) ->
	token_init_var(T, Ets, FilePath, Line - 1, FileName);
token_init_var([_ | T], Ets, FilePath, Line, FileName) ->
	token_init_var(T, Ets, FilePath, Line, FileName).
token_trans_var([], _, _, _Line, R) ->
	lists:reverse(R);
token_trans_var([{'{', _} = H | T], Ets, FilePath, Line, R) ->
	token_trans_var(T, Ets, FilePath, Line + 1, [H | R]);
token_trans_var([{'}', _} = H | T], Ets, FilePath, Line, R) ->
	token_trans_var(T, Ets, FilePath, Line - 1, [H | R]);
token_trans_var([{var, L, Var} | T], Ets, FilePath, Line, R) ->
	case get_var(Ets, Var, FilePath) of
		{ok, V} ->
			token_trans_var(T, Ets, FilePath, Line, [{string, L, V} | R]);
		none ->
			erlang:error([
				{err_token_trans_var, "var not found in cfg"},
				{var, Var}, {file, FilePath}])
	end;
token_trans_var([H | T], Ets, FilePath, Line, R) ->
	token_trans_var(T, Ets, FilePath, Line, [H | R]).

token_to_term(Token, Ets, FilePath) ->
	token_to_term(Token, 0, Ets, FilePath, [], []).
token_to_term([], 0, _Ets, _FilePath, [], R2) ->
	lists:reverse(R2);
token_to_term([], _Line, _Ets, FilePath, R, _R2) ->
	erlang:error([
		{err_token_to_term, "token incomplete"},
		{file, FilePath},
		{token, R}
	]);
token_to_term([{'}', Line} = H | T], 1, Ets, FilePath, R, R2) ->
	R1 = lists:reverse([{dot, Line}, H | R]),
	case erl_parse:parse_term(R1) of
		{ok, {Key, _} = Term} ->
			check_cfg_duplicate(Ets, Line, Key, FilePath),
			set_var(Ets, Key, FilePath),
			token_to_term(T, 0, Ets, FilePath, [], [{{FilePath, Line}, Term} | R2]);
		Err ->
			erlang:error([
				{err_token_to_term, Err},
				{token, R1}
			])
	end;
token_to_term([{'{', _} = H | T], Line, Ets, FilePath, R, R2) ->
	token_to_term(T, Line + 1, Ets, FilePath, [H | R], R2);
token_to_term([{'}', _} = H | T], Line, Ets, FilePath, R, R2) ->
	token_to_term(T, Line - 1, Ets, FilePath, [H | R], R2);
token_to_term([{dot, _} | T], Line, Ets, FilePath, R, R2) ->
	token_to_term(T, Line, Ets, FilePath, R, R2);
token_to_term([H | T], Line, Ets, FilePath, R, R2) ->
	token_to_term(T, Line, Ets, FilePath, [H | R], R2).
%% 获取变量值
get_var(Ets, Var, FilePath) ->
	Key = {FilePath, Var},
	case ets:lookup(Ets, Key) of
		[{_, {M, F, A}}] ->
			NowA = get_var_(A, Ets, FilePath, []),
			NowV = M:F(NowA),
			ets:insert(Ets, {Key, {ok, NowV}}),
			{ok, NowV};
		[{_, {ok, V}}] ->
			{ok, V};
		[] ->
			case ets:lookup(Ets, Var) of
				[{_, {M, F, A}}] ->
					NowA = get_var_(A, Ets, FilePath, []),
					NowV = apply(M, F, NowA),
%%					NowV = M:F(NowA),
					ets:insert(Ets, {Var, {ok, NowV}}),
					{ok, NowV};
				[{_, {ok, V}}] ->
					{ok, V};
				[] ->
					none
			end
	end.
get_var_([H | T], Ets, FilePath, Result) ->
	Now =
		case wx_overman_lib:is_variant(H) of
			true ->
				case get_var(Ets, H, FilePath) of
					{ok, V} ->
						V;
					none ->
						erlang:error([
							{err_token_trans_var, "var not found in cfg"},
							{var, H}, {file, FilePath}])
				end;
			_ ->
				H
		end,
	get_var_(T, Ets, FilePath, [Now | Result]);
get_var_([], _, _, Result) ->
	lists:reverse(Result).

set_var(Ets, Var, FilePath) ->
	set_var(Ets, ?Var_Global, Var, [], FilePath).
set_var(Ets, VarSign, Var, FilePath, Val) ->
	Key = make_var_key(VarSign, Var, FilePath),
	ets:insert(Ets, {Key, Val}).

make_var_key(var_l, Var, FilePath) ->
	{FilePath, Var};
make_var_key(var_g, Var, _FilePath) ->
	Var.

check_cfg_duplicate(_Ets, _Line, {_, _}, _FilePath) ->
	ok.
%%check_cfg_duplicate(_Ets, _Line, {?IsIgnore_Boot, _}, _FilePath) ->
%%	ok;
%%check_cfg_duplicate(Ets, Line, Key, FilePath) ->
%%	case ets:member(Ets, Key) of
%%		false ->
%%			ok;
%%		_ ->
%%			[C] = ets:lookup(Ets, Key),
%%			erlang:error([{err_var_check_duplicate, "cfg dulicate"},
%%				{line, Line}, {key, Key}, {file, FilePath},
%%				{conflict_file, C}])
%%	end.

check_var_duplicate(Ets, VarSign, Var, FilePath, FileName) ->
	Key = make_var_key(VarSign, Var, FilePath),
	ets:member(Ets, Key) andalso
		erlang:error([{err_var_check_duplicate, "var dulicate"},
			{file_path, FilePath}, {file_name, FileName}, {var, Var}, {val, ets:lookup(Ets, Key)}]).

parse_var([{'}', _} = H | T], 0, Token, Result) ->
	Token1 = lists:reverse([{dot, 0}, H | Token]),
	case erl_parse:parse_term(Token1) of
		{ok, Term} ->
			{Term, T, [H | Result]};
		Err ->
			erlang:error([{err_parse_var, Err}, {token, Token1}])
	end;
parse_var([{'{', _} = H | T], Line, Token, Result) ->
	parse_var(T, Line + 1, [H | Token], [H | Result]);
parse_var([{'}', _} = H | T], Line, Token, Result) ->
	parse_var(T, Line - 1, [H | Token], [H | Result]);
parse_var([{var, L, Var} | T], Line, Token, Result) ->
	parse_var(T, Line, [{atom, L, Var} | Token], [{atom, L, Var} | Result]);
parse_var([H | T], Line, Token, Result) ->
	parse_var(T, Line, [H | Token], [H | Result]);
parse_var([], _Line, Token, _Result) ->
	erlang:error([{err_parse_var, "parse_var err is incomplete"},
		{token, lists:reverse(Token)}]).
%% -----------------------------------------------------------------
%% Func:
%% Description:文件分类
%% Returns:[{".cfg", ...}, {".beam", ...},...]
%% -----------------------------------------------------------------
sort_file(List) ->
	sort_file(List, []).
sort_file([{_FilePath, {_Name, Extension, _Size, _Mt, _Info}} = H | T], Result) ->
	List = proplists:get_value(Extension, Result, []),
	Result1 = lists:keystore(Extension, 1, Result, {Extension, [H | List]}),
	sort_file(T, Result1);
sort_file([], Result) ->
	Result.

%% -----------------------------------------------------------------
%% Func:
%% Description:配置分类
%% Returns:
%% -----------------------------------------------------------------
sort_cfg([{{_, _}, {{Type, _}, _}} = H | T], Result) ->
	List = proplists:get_value(Type, Result, []),
	Result1 = lists:keystore(Type, 1, Result, {Type, [H | List]}),
	sort_cfg(T, Result1);
sort_cfg([], Result) ->
	Result.

%% -----------------------------------------------------------------
%% Func:
%% Description:获取简单的‘CurProject'值
%% Returns:
%% -----------------------------------------------------------------
get_cur_project_path(Path) ->
	filename:dirname(filename:dirname(Path)).


%% -----------------------------------------------------------------
%% Func:
%% Description:初始化mf列表
%% Returns:
%% -----------------------------------------------------------------
init_mf([{_, {{_, Name}, Info}} | T], Result) ->
	init_mf(T, [{Name, Info} | Result]);
init_mf([], Result) ->
	Result.


%% -----------------------------------------------------------------
%% Func:
%% Description: 运行配置项
%% Returns:
%% -----------------------------------------------------------------
run(Boot, Mf, Cfg, Init, Other) ->
	ok = run_boot(lists:reverse(Boot), Mf),
	ok = run_cfg(Cfg, Mf),
	ok = run_other(Other, Mf),
	ok = run_init(Init, Mf).
run_init([{FileLine, {{Type, M}, A}} | T], MfCfg) ->
	case apply_mfa(M, Type, A, MfCfg, FileLine) of
		ok ->
			run_init(T, MfCfg);
		{ok, _} ->
			run_init(T, MfCfg);
		Err ->
			Err
	end;
run_init([H | T], MfCfg) ->
	run_init(H, MfCfg),
	run_init(T, MfCfg);
run_init([], _) ->
	ok.
run_other([{FileLine, {{M, _}, _} = A} | T], Mf) ->
	case apply_mfa(M, set, [A], Mf, FileLine) of
		ok ->
			run_other(T, Mf);
		{ok, _} ->
			run_other(T, Mf);
		Err ->
			Err
	end;
run_other([H | T], Mf) ->
	run_other(H, Mf),
	run_other(T, Mf);
run_other([], _Mf) ->
	ok.
run_boot([{FileLine, {{Type, M}, A}} | T], MfCfg) ->
	case apply_mfa(M, Type, A, MfCfg, FileLine) of
		ok ->
			run_boot(T, MfCfg);
		{ok, _} ->
			run_boot(T, MfCfg);
		Err ->
			Err
	end;
run_boot([H | T], Mf) ->
	run_boot(H, Mf),
	run_boot(T, Mf);
run_boot([], _Mf) ->
	ok.
run_cfg([{{FilePath, Line} = FileLine, {{Type, Name}, A}} | T], Mf) ->
	case apply_mfa(Type, set, [FilePath, Line, Name | A], Mf, FileLine) of
		ok ->
			run_cfg(T, Mf);
		Err ->
			Err
	end;
run_cfg([H | T], Mf) ->
	run_cfg(H, Mf),
	run_cfg(T, Mf);
run_cfg([], _) ->
	ok.

apply_mfa(M, Type, A, MfCfg, FileLine) ->
	case proplists:get_value(M, MfCfg, none) of
		none ->
			{err, {no_module_config, M, FileLine}};
		Info ->
			case proplists:get_value(Type, Info, none) of
				none ->
					{err, {no_func_config, Info, FileLine}};
				{M1, F} ->
					apply_(M1, F, A);
				F ->
					apply_(M, F, A)
			end
	end.
apply_(M, F, A) ->
	try
		case apply(M, F, A) of
			{ok, _} = R ->
				R;
			ok ->
				ok
		end
	catch
		E1:E2:E3 ->
			exit({err, {{type, E1}, {pattern, E2}, {stack, E3}}})
	end.

get_lib_dir() ->
	case os:type() of
		{unix, _} ->
			"./lib";
		{win32, _} ->
			"../lib"
	end.