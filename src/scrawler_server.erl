%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Danila Petrov <ddbihbka@gmail.com>
%%% Description : The gen_server template.
%%%
%%% Created :  30 Jan 2013 by Danila Petrov <ddbihbka@gmail.com>
%%%-------------------------------------------------------------------
-module(scrawler_server).

-behaviour(gen_server).

%% API
-export([start_link/0, grab_main_calendar/1, save_img/1, grab_team_edata/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).

-define(MAIN_URL, "http://www.soccer.ru").
-define(URL(COUNTRY), ?MAIN_URL ++ "/" ++ COUNTRY++ "/games").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({grab, Url}, _From, State) ->
    {ok, {_State, _Headers, Body}} = httpc:request(Url),
    Tree = mochiweb_html:parse(Body),
    {reply, Tree, State};
handle_call({grab, Url, XpathTempl}, _From, State) ->
    {ok, {_State, _Headers, Body}} = httpc:request(Url),
    Tree = mochiweb_html:parse(Body),
    Results = mochiweb_xpath:execute(XpathTempl, Tree),
    {reply, Results, State};
handle_call({xpath, XpathTempl, Tree}, _From, State) ->
    {reply, mochiweb_xpath:execute(XpathTempl, Tree), State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({save_img, ImgSrc}, State) ->
  {ok, {_State, _Headers, Body}} = httpc:request(?MAIN_URL ++ ImgSrc),
  {ok, Path} = file:get_cwd(),
  file:write_file(Path ++ ImgSrc, Body), 
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
grab_main_calendar(Country) ->
    gen_server:call(?MODULE, {grab, ?URL(Country), "//table[@class='calendarTable']/tr"}, 10000). 

save_img(ImgSrc) ->
  gen_server:cast(?MODULE, {save_img, binary_to_list(ImgSrc)}). 

grab_team_edata(Href) ->
  Tree = gen_server:call(?MODULE, {grab, ?MAIN_URL ++ Href}, 10000),
  [{_, [{<<"src">>, TeamLogo},_,_], _}] = gen_server:call(?MODULE, {xpath, "//table[@class='maintable']/tr/td/table/tr/td/img", Tree}, 10000),
  [{_, _, Other}]  = gen_server:call(?MODULE, {xpath, "//table[@class='maintable']/tr[3]/td", Tree}, 10000),
  {TeamLogo, Other}.
  

