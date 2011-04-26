-module(em_player).
-behavior(gen_server).
-include("em_records.herl").

%% API
-export([start_link/1,
         stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% API
start_link(Player) ->
        gen_server:start_link({local,Player#player.key},
                ?MODULE,
                [Player], []).
stop(Key) -> gen_server:call(Key, stop).

%% gen_server
init([Player]) ->
        {ok, Player}.

handle_call(_Atom, _From, State) ->
        {reply, {ok, "Blah!"}, State}.

handle_cast(stop,State) ->
        {stop, normal, State};
handle_cast(_Msg, State) ->
        {noreply, State}.

handle_info(_Info, State) ->
        {noreply, State}.

terminate(_Reason, _State) ->
        ok.
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.
