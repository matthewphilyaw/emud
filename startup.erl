-module(startup).

-export([all/0]).

all() ->
    application:start(mnesia),
    application:start(ranch),
    application:start(crypto),
    application:start(cowboy),
    application:start(emud_db),
    application:start(emud_wire),
    application:start(emud),
    application:start(emud_cmd),
%%  TODO - Cowboy recently pushed the transport stuff into a project called ranch. emud_http needs to be updated.
    %%application:start(emud_http),
    application:start(emud_tcp),
    emud_db_install:install().
