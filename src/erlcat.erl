-module(erlcat).

-export([init/0, init_cat/1, get_cat_version/0, is_cat_enabled/0, destroy_cat/0]).
-export([log_event/4, log_error/2, log_metric_for_count/2, log_metric_for_duration/2, log_metric_for_sum/2, log_transaction_with_duration/3]).
-export([new_transaction/2, set_status/2, set_timestamp/2, set_duration/2, set_duration_start/2, add_data/2, add_kv/3, complete/1]).
-export([create_message_id/0, create_remote_message_id/1, get_message_tree_id/0, get_message_tree_root_id/0, get_message_tree_parent_id/0]).
-export([set_message_tree_id/1, set_message_tree_root_id/1, set_message_tree_parent_id/1]).

-on_load(init/0).

-define(APPNAME, erlcat).
-define(LIBNAME, erlcat).
-define(NOT_LOADED, not_loaded(?LINE)).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%% === Common Apis ===

init_cat(_AppKey) ->
    ?NOT_LOADED.

get_cat_version() ->
    ?NOT_LOADED.

is_cat_enabled() ->
    ?NOT_LOADED.

destroy_cat() ->
    ?NOT_LOADED.

%% === Event Apis ===

log_event(_Type, _Name, _Status, _Data) ->
    ?NOT_LOADED.

log_error(_Message, _ErrStr) ->
    ?NOT_LOADED.

%% === Metric Apis ===

log_metric_for_count(_Name, _Count) ->
    ?NOT_LOADED.

log_metric_for_duration(_Name, _Duration) ->
    ?NOT_LOADED.

log_metric_for_sum(_Name, _Value) ->
    ?NOT_LOADED.

%% === Transaction Apis ===

log_transaction_with_duration(_Type, _Name, _Duration) ->
    ?NOT_LOADED.

new_transaction(_Name, _Type) ->
	?NOT_LOADED.

set_status(_CatTransaction, _State)->
	?NOT_LOADED.

set_timestamp(_CatTransaction, _Timestamp)->
	?NOT_LOADED.

set_duration(_CatTransaction, _Duration)->
	?NOT_LOADED.

set_duration_start(_CatTransaction, _DurationStart)->
	?NOT_LOADED.

add_data(_CatTransaction, _Data)->
	?NOT_LOADED.

add_kv(_CatTransaction, _Key, _Value)->
	?NOT_LOADED.

complete(_CatTransaction)->
    ?NOT_LOADED.

%% === MessageId Apis ===    

create_message_id() ->
    ?NOT_LOADED. 

create_remote_message_id(_AppKey) ->
    ?NOT_LOADED. 

get_message_tree_id() ->
    ?NOT_LOADED. 

get_message_tree_root_id() ->
    ?NOT_LOADED. 

get_message_tree_parent_id() ->
    ?NOT_LOADED. 

set_message_tree_id(_MessageId) ->
    ?NOT_LOADED. 

set_message_tree_root_id(_MessageId) ->
    ?NOT_LOADED. 

set_message_tree_parent_id(_MessageId) ->
    ?NOT_LOADED.
