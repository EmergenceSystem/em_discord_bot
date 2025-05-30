-module(em_discord_bot_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    ok = application:set_env(discord_bot_light, command_handler, em_discord_bot_commands),
    {ok, _} = application:ensure_all_started(discord_bot_light),
    {ok, self()}.

stop(_State) ->
    ok.

