%% @private
%% @doc mDNS implementation for node discovery
-module(emnesia_mdns).

-behavior(gen_server).

-export([start_link/0,
         init/1,
         terminate/2,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(MDNS_ADDR, {224, 0, 0, 251}).
-define(MDNS_PORT, 5353).
-define(MDNS_DOMAIN, ".local").
-define(MDNS_TYPE, "_emnesia._tcp").
-define(MDNS_TTL, 120).

-record(state, {socket}).

%% @private
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @private
-spec init([term()]) -> term().
init([]) ->
    {ok, Socket} = create_socket(),
    ok = net_kernel:monitor_nodes(true),
    lager:info("Started emnesia_mdns"),
    {ok, #state{socket=Socket}}.

%% @private
handle_call(Request, From, State) ->
    io:format("Unknown call ~p from ~p", [Request, From]),
    {noreply, State}.

%% @private
handle_cast(announce, State) ->
    {ok, Names} = net_adm:names(),
    {ok, Hostname} = inet:gethostname(),
    Message = inet_dns:make_msg([{header, header()},
                                 {arlist, resources(Names, Hostname)}]),
    gen_udp:send(State#state.socket,
                 ?MDNS_ADDR, ?MDNS_PORT,
                 inet_dns:encode(Message)),
    lager:info("announcement sent"),
    {noreply, State};

%% @private
handle_cast(Request, State) ->
    io:format("Unknown cast ~p", [Request]),
    {noreply, State}.

%% @private
handle_info({udp, Socket, _IP, _InPort, Packet}, State) ->
    {ok, Record} = inet_dns:decode(Packet),
    Resources = [inet_dns:rr(RR) || RR <- inet_dns:msg(Record, arlist)],
    process_resources(Resources),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};

%% @private
handle_info(Info, State) ->
    io:format("Unknown info ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket),
    net_kernel:monitor_nodes(false),
    ok.

%% @private
code_change(_OldVsn, _Vsn, State) ->
    {ok, State}.

create_socket() ->
    gen_udp:open(?MDNS_PORT, [
        {reuseaddr, true},
        {ip, ?MDNS_ADDR},
        {multicast_ttl, 255},
        {multicast_loop, true},
        {multicast_if, {127, 0, 0, 1}},
        {broadcast, true},
        {add_membership, {?MDNS_ADDR, {0, 0, 0, 0}}},
        {active, once},
        binary]).

header() ->
    inet_dns:make_header([{id,0},
                          {qr,true},
                          {opcode,'query'},
                          {aa,true},
                          {tc,false},
                          {rd,false},
                          {ra,false},
                          {pr,false},
                          {rcode,0}]).

resources(Names, Hostname) ->
    services(Names, Hostname) ++ texts(Names, Hostname).

services(Names, Hostname) ->
    [inet_dns:make_rr([{domain, instance(Node, Hostname)},
		       {type, srv},
		       {class, in},
		       {ttl, ?MDNS_TTL},
		       {data, {0, 0, Port, Hostname ++ ?MDNS_DOMAIN}}]) || {Node, Port} <- Names].

texts(Names, Hostname) ->
    [inet_dns:make_rr([{domain, instance(Node, Hostname)},
                       {type, txt},
                       {class, in},
                       {ttl, ?MDNS_TTL},
                       {data, ["node=" ++ Node,
                               "hostname=" ++ net_adm:localhost(),
                               "port=" ++ integer_to_list(Port)]}]) || {Node, Port} <- Names].

instance(Node, Hostname) ->
    Node ++ "@" ++ Hostname ++ "." ++ ?MDNS_TYPE ++ ?MDNS_DOMAIN.

process_resources([]) -> ok;
process_resources([R|Resources]) ->
    lager:info("Domain: ~p", [proplists:get_value(domain, R)]),
    lager:info("Type: ~p", [proplists:get_value(type, R)]),
    lager:info("Data: ~p", [proplists:get_value(data, R)]),
    process_resources(Resources).
