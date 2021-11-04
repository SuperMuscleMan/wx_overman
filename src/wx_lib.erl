-module(wx_lib).
%%%=======================STATEMENT====================
-description("wx_lib").
-copyright('').
-author("wmh, SuperMuscleMan@outlook.com").
-vsn(1).
%%%=======================EXPORT=======================
-export([ls/2, read_token/1, is_variant/1, get_value/2, get_value/3, get_values/2,
	integer_to_list/2, term_to_string/1, ls/1, put_unicode/1, template_string/2,
	getCurProject/3, l_to_a/2, l_to_a/1, rand/2, getCurProject/1, getCurProjectAtom/1, getCurProjectAtom/3,
	random_byte/1]).
%%%=======================INCLUDE======================

-include_lib("inets/include/httpd.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("wx_log_library/include/wx_log.hrl").
%%%=======================RECORD=======================

%%%=======================DEFINE=======================

%%%=================EXPORTED FUNCTIONS=================
%% -----------------------------------------------------------------
%% Func:
%% Description:随机生成指定Byte的二进制
%% Returns:
%% -----------------------------------------------------------------
random_byte(Num) ->
	crypto:strong_rand_bytes(Num).


%% -----------------------------------------------------------------
%% Func:
%% Description:获取随机数
%% Returns:
%% -----------------------------------------------------------------
rand(N1, N2) when N2 > N1 ->
	N = (N2 - N1) + 1,
	rand:uniform(N) - 1 + N1.

%% -----------------------------------------------------------------
%% Func:
%% Description:获取当前项目文件名
%% Returns:list()
%% -----------------------------------------------------------------
getCurProject([Path, Reg, Match]) ->
	getCurProject(Path, Reg, Match).
getCurProject(Path, Reg, Match) ->
	ProjectDir = filename:basename(Path),
	case re:run(ProjectDir, Reg, Match) of
		{match, [R]} -> R;
		_ ->
			ProjectDir
	end.

%% -----------------------------------------------------------------
%% Func:
%% Description:获取当前项目文件名
%% Returns:atom()
%% -----------------------------------------------------------------
getCurProjectAtom([Path, Reg, Match]) ->
	getCurProjectAtom(Path, Reg, Match).
getCurProjectAtom(Path, Reg, Match) ->
	list_to_atom(getCurProject(Path, Reg, Match)).
%% -----------------------------------------------------------------
%% Func:
%% Description:拼接字符串（用指定参数替换模板中的'$v')
%% Returns:
%% -----------------------------------------------------------------
template_string(Template, Strings) ->
	R = template_string_(Template, Strings, []),
	lists:reverse(R).
template_string_([$$, $v | T], [H | T1], R) ->
	template_string_(T, T1, [H | R]);
template_string_([$$, $v | T], [], R) ->
	template_string_(T, [], [$l, $i, $n | R]);
template_string_([H | T], T1, R) ->
	template_string_(T, T1, [H | R]);
template_string_([], _, R) ->
	R.

%% -----------------------------------------------------------------
%% Func:
%% Description:erlang数据结构 转换为 String表达式
%% Returns:
%% -----------------------------------------------------------------
term_to_string(E) ->
	lists:reverse(term_to_string(E, [])).
term_to_string(E, R) when is_atom(E) ->
	[erlang:atom_to_list(E) | R];
term_to_string(E, R) when is_pid(E) ->
	[erlang:pid_to_list(E) | R];
term_to_string(E, R) when is_port(E) ->
	[erlang:port_to_list(E) | R];
term_to_string(E, R) when is_reference(E) ->
	[erlang:ref_to_list(E) | R];
term_to_string(E, R) when is_integer(E) ->
	[erlang:integer_to_list(E) | R];
term_to_string(E, R) when is_float(E) ->
	[erlang:float_to_list(E) | R];
term_to_string(E, R) when is_tuple(E) ->
	L = erlang:tuple_to_list(E),
	[$} | list_to_string(L, [${ | R])];
term_to_string(E, R) when is_binary(E) ->
	[$>, $> | list_to_string(erlang:binary_to_list(E), [$<, $< | R])];
term_to_string(E, R) when is_list(E) ->
	case check_abnormal_string(E) of
		true when E == [] ->
			[$], $[ | R];
		true ->
			[$] | list_to_string(E, [$[ | R])];
		false ->
			[$" | str_to_string(E, [$" | R])]
	end;
%% 	case [C || C <- E, C > 127 orelse C < 32] of
%%		[] when E == [] ->
%%			[$], $[ | R];
%%		[] ->
%%			[$" | str_to_string(E, [$" | R])];
%%		_ ->
%%			[$] | list_to_string(E, [$[ | R])]
%%	end;
term_to_string(E, R) ->
	Bin = erlang:term_to_binary(E),
	term_to_string(Bin, R).
%% 将[] 转换为string表达式
list_to_string([], R) ->
	R;
list_to_string([H], R) ->
	term_to_string(H, R);
list_to_string([H | T], R) ->
	R1 = term_to_string(H, R),
	list_to_string(T, [$, | R1]);
list_to_string(Abnormal, R) ->
	term_to_string(Abnormal, R).
%% 将“” 转换为 string表达式
str_to_string([], R) ->
	R;
str_to_string([H | T], R) ->
	str_to_string(T, [H | R]).

check_abnormal_string([H | _T]) when H > 127; H < 32 ->
	true;
check_abnormal_string([_H | T]) ->
	check_abnormal_string(T);
check_abnormal_string([]) ->
	false;
check_abnormal_string(_) ->
	true.
%% -----------------------------------------------------------------
%% Func:
%% Description:整数转换字符串 指定占位个数
%% Returns:
%% -----------------------------------------------------------------
integer_to_list(Int, Count) when Int < 1 ->
	integer_to_list_(integer_to_list(Int), Count - 1);
integer_to_list(Int, Count) ->
	Place = Count - (erlang:trunc(math:log10(Int)) + 1),
	integer_to_list_(integer_to_list(Int), Place).
integer_to_list_(Base, Count) when Count < 1 ->
	Base;
integer_to_list_(Base, Count) ->
	integer_to_list_([$0 | Base], Count - 1).
%% -----------------------------------------------------------------
%% Func:
%% Description:列表转atom
%% Returns:
%% -----------------------------------------------------------------
l_to_a(List) when is_list(List) ->
	erlang:list_to_atom(lists:concat(List)).
l_to_a(L1, L2) ->
	erlang:list_to_atom(L1 ++ L2).

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
%% Description:根据key获取value（只符合{key,value}格式
%% Returns:
%% -----------------------------------------------------------------
get_value(List, Key) ->
	get_value(List, Key, none).
get_value(List, Key, Default) when is_list(List) ->
	case lists:keyfind(Key, 1, List) of
		{_, Value} ->
			Value;
		false ->
			Default
	end.

get_values(List, KeyList) when is_list(List) ->
	get_values_(List, KeyList, []).
get_values_(_List, [], Result) ->
	Result;
get_values_(List, [{Key, Default} | T], Result) ->
	Value = get_value(List, Key, Default),
	get_values_(List, T, [{Key, Value} | Result]);
get_values_(List, [Key | T], Result) ->
	Value = get_value(List, Key),
	get_values_(List, T, [{Key, Value} | Result]).

%% -----------------------------------------------------------------
%% Func:
%% Description: 输出unicode码
%% Returns:
%% -----------------------------------------------------------------
put_unicode(S) when is_list(S) ->
	put_unicode(unicode:characters_to_binary(S));
put_unicode(S) when is_binary(S) ->
	S1 = unicode:characters_to_list(S),
	io:format("~ts~n", [S1]);
put_unicode(S) ->
	S1 = wx_lib:term_to_string(S),
	put_unicode(S1).
%==========================DEFINE=======================