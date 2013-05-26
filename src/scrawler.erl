%%Feel free to use, reuse and abuse the code in this file.

-module(scrawler).

%% API.
-export([start/0, extract/1, get_extra_data_about_team_and_save/3]).


-define(POOL_ID, pool1).
-record(team, {name, href, imgsrc}).
-record(teams, {team1 = #team{}, team2 = #team{}}).

%% API.

start() ->
    d_util:start_deps(?MODULE),
    application:start(emongo),
    emongo:add_pool(?POOL_ID, "localhost", 27017, "testdatabase", 1).


extract(Country) ->
    extract(scrawler_server:grab_main_calendar(Country), Country).

extract(Data, Country) ->
    get_teams_list([find_and_extract(H) || H <- Data], Country).


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


get_teams_list(List, Country) ->
    emongo:insert(?POOL_ID, Country, [get_teams(H, Country) || H <- List]).

get_teams([_, _, [ImgSrc, {Href, Name}], _, [{Href2, Name2}, ImgSrc2]], Country) ->
    scrawler_server:save_img(ImgSrc),
    scrawler_server:save_img(ImgSrc2),
    get_extra_data_about_team_and_save(Name, Href, Country),
    get_extra_data_about_team_and_save(Name2, Href2, Country),
    [{"first_team", [{"name", Name}, {"href", Href}, {"imgsrc", ImgSrc}]}, 
        {"second_team", [{"name", Name2}, {"href", Href2}, {"imgsrc", ImgSrc2}]}];
get_teams([_, [ImgSrc, {Href, Name}], _, [{Href2, Name2}, ImgSrc2]], Country) ->
    scrawler_server:save_img(ImgSrc),
    scrawler_server:save_img(ImgSrc2),
    get_extra_data_about_team_and_save(Name, Href, Country),
    get_extra_data_about_team_and_save(Name2, Href2, Country),
    [{"first_team", [{"name", Name}, {"href", Href}, {"imgsrc", ImgSrc}]}, 
        {"second_team", [{"name", Name2}, {"href", Href2}, {"imgsrc", ImgSrc2}]}].

get_extra_data_about_team_and_save(Name, Href, Country) ->
    {TeamLogo, Other} = scrawler_server:grab_team_edata(binary_to_list(Href)),
    emongo:update(?POOL_ID, Country, [{"team_name", Name}], [{"team_name", Name}, {"team_logo", TeamLogo}, {"edata", get_data_from_other(lists:filter(fun filter_data_from_other/1, Other), [])}], true).

filter_data_from_other({<<"b">>, _, _}) -> 
    true;
filter_data_from_other({<<"a">>, _, _}) -> 
    true;
filter_data_from_other(Text) when is_binary(Text)  -> 
    true;
filter_data_from_other(_) ->
    false.


get_data_from_other([{<<"b">>, _, [Key]}, {<<"a">>, _, [Value]} | Other], S) ->
    get_data_from_other(Other, [{Key, Value}| S]);
get_data_from_other([{<<"b">>, _, [Key]}, Value | Other], S) when is_binary(Value) ->
    get_data_from_other(Other, [{Key, Value}| S]);
get_data_from_other([{<<"a">>, _, [Value]} | Other], S) ->
    get_data_from_other(Other, [{unicode:characters_to_binary("Тренер") , Value}| S]);
get_data_from_other([], S) ->
    S.