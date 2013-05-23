%%Feel free to use, reuse and abuse the code in this file.

-module(scrawler).

%% API.
-export([start/0, grab/0]).

%% API.

start() ->
    d_util:start_deps(?MODULE).


grab() ->
    scrawler_server:grab("spain").

