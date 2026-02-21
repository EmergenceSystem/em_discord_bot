%%%-------------------------------------------------------------------
%%% @doc Discord bot command handler for Emergence queries.
%%% @end
%%%-------------------------------------------------------------------
-module(em_discord_bot_commands).

-export([handle_message/4]).

%%====================================================================
%% Message handler
%%====================================================================

handle_message(Content, ChannelId, _Author, Token) ->
    Body = iolist_to_binary(
        json:encode(#{<<"value">> => unicode:characters_to_binary(Content)})),
    case query(Body) of
        {ok, Embryos} ->
            Msg = format_message(Embryos),
            discord_bot_light_client:send_message(ChannelId, iolist_to_binary(Msg), Token);
        {error, Reason} ->
            Err = io_lib:format("Search error: ~p", [Reason]),
            discord_bot_light_client:send_message(ChannelId, iolist_to_binary(Err), Token)
    end.

%%====================================================================
%% Formatting
%%====================================================================

format_message(Embryos) ->
    lists:map(fun(E) ->
        Props = maps:get(<<"properties">>, E, #{}),
        Url   = maps:get(<<"url">>,    Props, <<>>),
        Desc  = maps:get(<<"resume">>, Props, <<>>),
        io_lib:format("~ts~n~ts~n~n", [Url, Desc])
    end, lists:sublist(Embryos, 5)).

%%====================================================================
%% HTTP query
%%====================================================================

query(Body) ->
    ServerUrl = embryo:get_em_disco_url(),
    Url       = ServerUrl ++ "/query",
    Headers   = [{"content-type", "application/json"}],
    case httpc:request(post, {Url, Headers, "application/json", Body},
                       [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, RespBody}} ->
            handle_response(RespBody);
        {ok, {{_, Status, _}, _, RespBody}} ->
            io:format("HTTP ~p from ~s~n", [Status, Url]),
            {error, RespBody};
        {error, Reason} ->
            {error, Reason}
    end.

handle_response(Body) ->
    try json:decode(Body) of
        #{<<"embryo_list">> := EmbryoList} -> {ok, EmbryoList};
        _                                  -> {ok, []}
    catch
        _:_ -> {error, invalid_json}
    end.
