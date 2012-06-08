-module (emud_init).
-include("../include/emud.hrl").

-export([init/0]).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),

    mnesia:create_table(usr, [{attributes, record_info(fields, usr)}]),
    mnesia:create_table(char, [{attributes, record_info(fields, char)}]),
    ok.