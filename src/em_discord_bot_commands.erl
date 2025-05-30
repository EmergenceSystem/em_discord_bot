-module(em_discord_bot_commands).
-export([handle_message/4]).

-include_lib("embryo/src/embryo.hrl").

%%--------------------------------------------------------------------
%% Main message handler
%%--------------------------------------------------------------------
handle_message(Content, ChannelId, _Author, Token) ->
    SearchQuery = Content,
    io:format("Search Query: ~p~n", [SearchQuery]),

    Results = query(SearchQuery),
    case Results of
        {error, Reason} ->
            ErrorMsg = io_lib:format("Search error: ~p", [Reason]),
            io:format("Sending Error Message: ~p~n", [ErrorMsg]),
            discord_bot_light_client:send_message(ChannelId, iolist_to_binary(ErrorMsg), Token);
        {ok, MessageList} ->
            FormattedMessage = format_message(MessageList),
            discord_bot_light_client:send_message(ChannelId, iolist_to_binary(FormattedMessage), Token)
    end.

%% Function to format the message list into a single string
format_message(MessageList) ->
    TruncatedList = lists:sublist(MessageList, 5),
    lists:map(fun(Message) ->
        Url = get_url(Message),
        Description = get_description(Message),
        io_lib:format("~s~n~s~n~n", [Url, Description])
    end, TruncatedList).

%% Function to get the URL and description from the map
get_embryo_info(EmbryoMap) ->
    Props = maps:get(<<"properties">>, EmbryoMap, #{}),
    Url = maps:get(<<"url">>, Props, ""),
    Description = maps:get(<<"description">>, Props, ""),
    #{url => Url, description => Description}.

%%%-------------------------------------------------------------------
%%% Query Logic
%%%-------------------------------------------------------------------
query(Search) ->
    ServerUrl = embryo:get_em_disco_url(),
    io:format("Server URL: ~s~n", [ServerUrl]),
    handle_query(ServerUrl, Search).

%%%-------------------------------------------------------------------
%%% Send query and handle response
%%%-------------------------------------------------------------------
handle_query(ServerUrl, Query) ->
    case post_query(ServerUrl, Query) of
        {ok, Body} ->
            handle_response(Body);
        {error, Reason} ->
            io:format("Failed to send HTTP request to ~s : ~p~n", [ServerUrl, Reason]),
            {error, Reason}
    end.

%% Function to perform a POST query
post_query(ServerUrl, Query) ->
    URL = ServerUrl ++ "/query",
    Headers = [{"content-type", "application/json"}],
    Body = Query,
    case httpc:request(post, {URL, Headers, "application/json", Body}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
            {ok, binary_to_list(RespBody)};
        {ok, {{_, Status, _}, _, RespBody}} ->
            io:format("Failed to get successful response. Status code: ~p~n", [Status]),
            {error, binary_to_list(RespBody)};
        {error, Reason} ->
            {error, Reason}
    end.

%%%-------------------------------------------------------------------
%%% Response handling
%%%-------------------------------------------------------------------
handle_response(Body) ->
    case jsx:is_json(list_to_binary(Body)) of
        true ->
            try jsx:decode(list_to_binary(Body), [return_maps]) of
                #{<<"embryo_list">> := EmbryoList} ->
                    Messages = lists:map(fun get_embryo_info/1, EmbryoList),
                    {ok, Messages};
                _ ->
                    {ok, [Body]}
            catch
                _:_ ->
                    {ok, [Body]}
            end;
        false ->
            {ok, [Body]}
    end.

%% Function to get the description
get_description(Message) ->
    case Message of
        #{description := Description} when is_binary(Description) ->
            binary_to_list(Description);
        #{description := Description} ->
            Description;
        _ ->
            "No Description"
    end.

%% Function to get the URL
get_url(Message) ->
    case Message of
        #{url := Url} when is_binary(Url) ->
            binary_to_list(Url);
        #{url := Url} ->
            Url;
        _ ->
            "No URL"
    end.

