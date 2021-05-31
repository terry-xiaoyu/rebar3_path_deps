-module(rebar_path_resource).

-export([init/2,
         lock/2,
         download/4, download/3,
         needs_update/2,
         make_vsn/1]).

-include_lib("kernel/include/file.hrl").

init(Type, _State) ->
   Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
   {ok, Resource}.

lock(Dir, Source) when is_tuple(Source) ->
  lock_(Dir, Source);

lock(AppInfo, _) ->
  lock_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

lock_(Dir, {app, Path, _}) ->
  lock_(Dir, {app, Path});

lock_(_Dir, {app, Path}) ->
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),
  {app, Path, {mtime, to_iso8601(last_modified(Source))}}.

download(TmpDir, {app, Path, _}, State) ->
  download(TmpDir, {app, Path}, State);
download(TmpDir, {app, Path}, State) ->
  case download_(TmpDir, {app, Path}, State) of
    ok -> {ok, State};
    Error -> Error
  end.

download(TmpDir, AppInfo, State, _) ->
  case rebar_app_info:source(AppInfo) of
    {app, Path} ->
      download_(TmpDir, {app, Path}, State);
    {app, Path, _} ->
      download_(TmpDir, {app, Path}, State)
  end.

download_(Dir, {app, Path}, _State) ->
  ok = filelib:ensure_dir(Dir),
  {ok, Cwd} = file:get_cwd(),
  Source = filename:join([Cwd, Path]),
  rebar_log:log(info, "checking app deps at: ~p, dir=~p ~n", [Path, Dir]),
  filelib:is_file(Source).

make_vsn(_Dir) ->
  {error, "Replacing version of type app not supported."}.

needs_update(_AppInfo, _) ->
  true.

last_modified(Source) ->
  Files = filter_files(dir_files(Source)),
  last_modified_(Files).

last_modified_([]) -> calendar:local_time();
last_modified_(Files) ->
  lists:foldl(
    fun(Path, OldT) ->
        T = filelib:last_modified(Path),
        if
          T > OldT -> T;
          true -> OldT
        end
    end,
    0,
    Files).

to_iso8601({{Y,Mo,D}, {H,Mn,S}}) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    list_to_binary(IsoStr).


dir_files(Path) ->
  case filelib:is_dir(Path) of
    true ->
      filelib:wildcard(filename:join(Path, "**"));
    false ->
      [Path]
  end.


filter_files(Files) ->
    lists:filter(fun is_excluded/1, [filename:absname(F) || F <- Files]).


is_excluded(Path) ->
      KnownExcludes = [
                     "^.",
                     "~$"
                      ],

      lists:foldl(fun(_, true) -> true;
                     (RE, false) ->
                      (re:run(Path, RE, [unicode]) =/= nomatch) orelse (filelib:is_regular (Path) /= true)
                  end, false, KnownExcludes).

copy(File, Source, Target) ->
    SourceFile = filename:join([Source | File]),
    TargetFile = filename:join([Target | File]),
    ok = filelib:ensure_dir(TargetFile),
    {ok, _} = file:copy(SourceFile, TargetFile).

%%
%% applies a function to each file for its side-effects
foreach(Fun, Ignore, Path) ->
    foreach(Fun, Path, Ignore, []).

foreach(Fun, Root, Ignore, Path) ->
    File = filename:join([Root | Path]),
    case filelib:is_dir(File) of
        true  ->
            case file:list_dir(File) of
                {ok, List} ->
                    lists:foreach(
                        fun(X) ->
                            foreach(Fun, Root, Ignore, X)
                        end,
                        [Path ++ [X] || X <- List, not lists:member(X, Ignore)]
                    );
                {error, _Reason} ->
                   ok
            end;
        false -> 
            Fun(Path)
    end.

