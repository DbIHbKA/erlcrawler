%%Feel free to use, reuse and abuse the code in this file.

-module(scrawler).

%% API.
-export([start/0, grab_raw_data/0, extract/0]).


-record(team, {name, href, imgsrc}).
-record(teams, {team1 = #team{}, team2 = #team{}}).

%% API.

start() ->
    d_util:start_deps(?MODULE).


grab_raw_data() ->
    scrawler_server:grab("spain").

extract() ->
    Data = grab_raw_data(),
    extract(Data).

extract(Data) ->
    get_teams_list([find_and_extract(H) || H <- Data]).


find_and_extract({<<"tr">>, _, Tds}) ->
    [find_and_extract(H) || H <- Tds];
find_and_extract({<<"td">>, _, AandImg}) -> 
    [find_and_extract(H) || H <- AandImg];
find_and_extract({<<"a">>, [{<<"href">>, Href}, _], [Text]}) when is_binary(Text)  ->
    {Href, Text};
find_and_extract({<<"img">>, [{<<"src">>, Imgsrc}, _, _], _}) ->
    Imgsrc;
find_and_extract({<<"img">>, [{<<"src">>, Imgsrc}, _, _, _], _}) ->
    Imgsrc;
find_and_extract(_) ->
    ok.


get_teams_list(List) ->
    [get_teams(H) || H <- List].

get_teams([_, _, [ImgSrc, {Href, Name}], _, [{Href2, Name2}, ImgSrc2]]) ->
    #teams{team1 = #team{name = Name, href = Href, imgsrc = ImgSrc}, team2 = #team{name = Name2, href = Href2, imgsrc = ImgSrc2}};
get_teams([_, [ImgSrc, {Href, Name}], _, [{Href2, Name2}, ImgSrc2]]) ->
    #teams{team1 = #team{name = Name, href = Href, imgsrc = ImgSrc}, team2 = #team{name = Name2, href = Href2, imgsrc = ImgSrc2}}.


