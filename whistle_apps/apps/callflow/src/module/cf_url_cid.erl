%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%% Gets a caller id to display out of the database based on the configuration of the documents in the db
%%% @end
%%% @contributors
%%%   Ethan Brooks
%%%   Will Ferrer
%%%-------------------------------------------------------------------

-module(cf_url_cid).

-include("../callflow.hrl").

-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    lager:debug("will -- cf_url_cid:handle Data: ~p, Call: ~p", [Data, Call]),
    % Get all the info about the callflow -- had to make some of these be get_value because the ne flavor wouldn't do it:
    % Get call flow data:
    {CFCID, CFCIDName, CFUseDefaultCallerId, CFCampaign, CFUseDefaultCampaign} = get_call_flow_data(Data),

    % Get all the info about the call:
    {CallCaptureGroup, CallCID, CallCIDName} = get_call_data(Call),

    % Generate dial call area code
    AreaCode = get_area_code(CallCaptureGroup),

    % Select a campaign, either the caller_id_number of the calling party or the default one for the callflow
    Campaign = case CFUseDefaultCampaign of
        true ->
            CFCampaign;
        _ ->
            CallCID
    end,

    % Get caller id name from call flow or call
    OutdoundCIDName = case CFCIDName of
       undefined ->
           CallCIDName;
       _ ->
           CFCIDName
    end,

    % Generate a fall back CID if one can't be found in the database
    FallBackCID = case CFCID of
        undefined ->
            CallCID;
         _ ->
            CFCID
    end,

    lager:debug("will -- cf_url_cid:handle got values, CFCID: ~p, CFCIDName: ~p, CFUseDefaultCallerId: ~p, CFCampaign: ~p, CFUseDefaultCampaign: ~p, CallCaptureGroup: ~p, CallCID: ~p, CallCIDName: ~p, AreaCode: ~p, Campaign: ~p, OutdoundCIDName: ~p, FallBackCID: ~p", [CFCID, CFCIDName, CFUseDefaultCallerId, CFCampaign, CFUseDefaultCampaign, CallCaptureGroup, CallCID, CallCIDName, AreaCode, Campaign, OutdoundCIDName, FallBackCID]),


    % Get outbound cid based on the configuration of the db objects
    OutboundCID = select_outbound_cid(AreaCode, Campaign, Call, CFUseDefaultCallerId, FallBackCID, CallCID),

    lager:info("setting the caller id number to new num: ~s name: ~s", [OutboundCID, OutdoundCIDName]),

    {ok, C1} = cf_exe:get_call(Call),
    Updates = [
               fun(C) -> whapps_call:kvs_store(dynamic_cid, OutboundCID, C) end
               ,fun(C) -> whapps_call:set_caller_id_number(OutboundCID, C) end
               ,fun(C) -> whapps_call:set_caller_id_name(OutdoundCIDName, C) end
              ],
    cf_exe:set_call(whapps_call:exec(Updates, C1)),
    cf_exe:continue(Call).

-spec get_call_flow_data(wh_json:object()) -> any().
get_call_flow_data(Data) ->
    {wh_json:get_ne_value(<<"caller_id_number">>, Data),
    wh_json:get_ne_value(<<"caller_id_name">>, Data),
    wh_json:get_value(<<"use_default_caller_id">>, Data),
    wh_json:get_ne_value(<<"campaign">>, Data),
    wh_json:get_value(<<"use_default_campaign">>, Data)}.

-spec get_call_data(whapps_call:call()) -> any().
get_call_data(Call) ->
    {whapps_call:kvs_fetch(cf_capture_group, Call),
    whapps_call:caller_id_number(Call),
    whapps_call:caller_id_name(Call)}.

-spec get_area_code(binary()) -> any().
get_area_code(Number) ->
    string:substr(binary_to_list(Number), 1, 4).


-spec select_outbound_cid(string(), string(), whapps_call:call(), boolean(), string(), string()) -> any().
select_outbound_cid(AreaCode, Campaign, Call, CFUseDefaultCallerId, FallBackCID, CallCID) ->
    % Generate key for view query
    ViewKey = list_to_binary(string:concat(string:concat(AreaCode,"|"),Campaign)),
    ViewOptions = [{key, ViewKey}],

    % Get account for call:
    AccountDb = whapps_call:account_db(Call),

    % Get outbound cid based on the configuration of the db objects
    OutboundCID = case CFUseDefaultCallerId of
        true ->
            lager:info("will -- using fall back cid: ~p", [FallBackCID]),
            FallBackCID;
        _ ->
            case couch_mgr:get_results(AccountDb, <<"numbers/status">>, ViewOptions) of
                {ok, []} ->
                     lager:info("will -- number doesnt exist with key: ~p", [ViewOptions]),
                     CallCID;
                {ok, [JObj]} ->
                     lager:info("will -- got profile of ~p ViewOptions: ~p", [JObj, ViewKey]),
                     NumbersList = wh_json:get_value(<<"value">>, JObj),
                     lager:debug("will -- got numbers list: ~p, list length: ~p", [NumbersList, length(NumbersList)]),
                     {A1,A2,A3} = now(),
                     random:seed(A1, A2, A3),
                     Index = random:uniform(length(NumbersList)),
                     lager:debug("will -- getting number of index: ~p", [Index]),
                     lists:nth(Index, NumbersList);
                {ok, _} ->
                    lager:info("will -- result is ambiguous with key", [ViewKey]),
                    CallCID;
                 _E ->
                    lager:info("failed to find number with key ~P error: ~p", [ViewKey, _E])
            end
     end.

