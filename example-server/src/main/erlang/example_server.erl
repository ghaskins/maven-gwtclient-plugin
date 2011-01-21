%%% -------------------------------------------------------------------
%%% Author  : ghaskins
%%% Description :
%%%
%%% Created : Jan 18, 2011
%%% -------------------------------------------------------------------
-module(example_server).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, out/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

out(Arg) ->
    Req = Arg#arg.req,
    Headers = Arg#arg.headers,
    ReqPath = string:tokens(Arg#arg.server_path, "/"),
    Accepts = string:tokens(Headers#headers.accept, ","),
    process(Req#http_request.method, Accepts, ReqPath, Arg).

process(Cmd, [Accept | T], Request, Arg) ->
    try
	["api" | ReqPath] = Request, %% all paths should be prepended with /api
	handle_request(Cmd, Accept, ReqPath, Arg)
    catch
        throw:nomatch -> process(Cmd, T, Request, Arg)
    end;                                         
process(Cmd, [], Request, Arg) ->
    error_logger:info_msg("Ignoring ~p ~p (~p)~n",
                          [Cmd, Request, Arg]),
    make_response(404, "<p>Page not found</p>").

handle_request('GET', "application/json", ["hello"], Arg) ->
    P = yaws_api:parse_query(Arg),
    case lists:keysearch("user", 1, P) of
	{value, {"user", User}} ->
	    Headers = Arg#arg.headers,
	    Agent = Headers#headers.user_agent,
	    ServerInfo = "Erlang/Yaws",
	    Message = {struct, [{user, User},
				{userAgent, Agent},
				{serverInfo, ServerInfo}]},
	    EncodedMessage = mochijson:encode(Message),
	    make_response(200, "application/json", EncodedMessage);
	BadResult ->
	    io:format("Unexpected result: ~p from ~p~n", [BadResult, P]),
	    make_response(404, "text/plain", "Missing ?user=$user")
    end;

handle_request(Cmd, Accept, Request, Arg) -> % catchall
    throw(nomatch).

make_response(Status, Message) ->
    make_response(Status, "text/html", Message).

make_response(Status, Type, Message) ->
    make_all_response(Status, make_header(Type), Message).

make_header(Type) ->
    [{header, ["Content-Type: ", Type]}].

make_all_response(Status, Headers, Message) ->
    [{status, Status}, {allheaders, Headers}, {html, Message}].

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    PrivDir = code:priv_dir(server),

    GC = yaws_config:make_default_gconf(false, "example"),
    SC = #sconf{
      port = 8001,
      servername = "localhost",
      listen = {0, 0, 0, 0},
      docroot = PrivDir ++ "/webapp",
	  %% redirect_map = [{"/", #url{path="/example.html"}, noappend}],
      appmods = [{"/api", ?MODULE}]
    },

    case catch yaws_api:setconf(GC, [[SC]]) of
        ok -> {ok, started};
        Error -> {stop, Error}
    end,

    {ok, #state{}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

