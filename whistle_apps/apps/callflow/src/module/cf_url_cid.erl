%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Jon Blanton
%%%-------------------------------------------------------------------
-module(cf_url_cid).

-include("../callflow.hrl").

-export([handle/2]).
-define(GET_CONFIG, fun(Key, Default) ->
                      whapps_config:get_binary(<<"callflow.dynamic_cid">>, Key, Default)
                    end).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".dynamic_cid">>).

-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    {CID, CIDName} =
        case wh_json:get_ne_value(<<"action">>, Data) of
            <<"clear">> -> {'undefined', 'undefined'};
            _ ->
                 {wh_json:get_ne_value(<<"caller_id_number">>, Data)
		,wh_json:get_ne_value(<<"caller_id_name">>, Data)}
        end,

%%            Prefix = string:substr('+18052842586', 1, 4),
            Prefix = <<"1805">>,
            ViewOptions = [{key, Prefix}],
            AccountDb = whapps_call:account_db(Call),

            NewCID = case couch_mgr:get_results(AccountDb, <<"numbers/status">>, ViewOptions) of
                {ok, []} ->
                    lager:info("number ~s doesnt exist with key: ~p", [CID, ViewOptions]);
                {ok, [JObj]} ->
                    lager:info("get profile of ~p", [JObj]),
                    wh_json:get_value(<<"value">>, JObj);
                {ok, _} ->
                    lager:info("number ~s is ambiguous", [CID]);
                _E ->
                    lager:info("failed to find number ~s error: ~p", [CID, _E])
            end,

    lager:info("setting the caller id number to new num: ~s  num: ~s name: ~s", [NewCID, CID, CIDName]),

    {ok, C1} = cf_exe:get_call(Call),
    Updates = [
               fun(C) -> whapps_call:kvs_store(dynamic_cid, NewCID, C) end
               ,fun(C) -> whapps_call:set_caller_id_number(NewCID, C) end
               ,fun(C) -> whapps_call:set_caller_id_name(CIDName, C) end
              ],
    cf_exe:set_call(whapps_call:exec(Updates, C1)),
    cf_exe:continue(Call).
