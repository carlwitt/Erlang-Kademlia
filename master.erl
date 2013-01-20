%%%      Module : master
%%%      Author : Carl Witt
%%% Description : defines the metric used to compare keys
%%%     Created : 04 Aug 2011 by Carl Witt
%%%
%%% Abstract: The master module contains all controls to start, stop and
%%% influence the simulation. It spawns and removes nodes.

-module(master).
-include("include/systemMonitor.hrl").
-include("include/kademliaGlobal.hrl").
-include("include/node.hrl").

-export([
			start/1,
			spawnNode/1,
			spawnN/1,
			muteFind/0,
			monSend/1,
			storeCapitals/1
		]).
-define(LOCAL_NAME, master).

start(MonitorNode) ->
    MasterPID = spawn(fun() -> master([]) end),
    
    Connect = net_kernel:connect(MonitorNode),
    io:format("~nConnect to monitor node ~p: ~p~n",[MonitorNode,Connect]),
	
    Now = httpd_util:rfc1123_date(erlang:universaltime()),
    Running = io_lib:format("~n~n== MASTER NODE running ~s (~p/~p) == ~n~n", [Now,MasterPID,?LOCAL_NAME]),
	io:format(Running),
	{mon, MonitorNode} ! reset,			% remove previous settings
	{mon, MonitorNode} ! Running,		% display a message on the monitor to mark the new "session"
    register(?LOCAL_NAME, MasterPID),
    timer:sleep(500),
	test().

% carries a list of #peer records as state.
master(Nodes) ->
    receive
	
	{spawnNode, _NodeNameUnused} ->

		NodeName = erlang:list_to_atom("node"++erlang:integer_to_list(length(Nodes))),

		% determine a node ID (key) for the new node
		% Key = metric:randomKey(Name),	
		Key = metric:calculateKey(erlang:atom_to_list(NodeName)),	% to see whether e.g. "north" is really stored on node north.

		% create a peer record for the newly spawned node
		You = #peer{name = NodeName, key = Key, spawnOrder = length(Nodes)},
		
		io:format("~sN~s ",[if length(Nodes) rem 10 == 0 -> "\n"; ?ELSE -> "" end, erlang:integer_to_list(length(Nodes))]),

		NodePID = node:start(You),
		register(NodeName, NodePID),
		
		% to join the network, each node needs an initial contact to announce itself in the network
		InitialNode = utils:selectRandom(Nodes),    
		% the case when no initial node exists is handled in the boot procedure.
		NodePID ! {boot,  InitialNode},
		
		?LOG_RAW("Spawned node ~p/~p and passed initial node ~p ~n",[NodePID, NodeName,InitialNode], ?JOINING_THE_NETWORK),
		NewNodes = Nodes++[You#peer{pid = NodePID}],
	    master(NewNodes);   
	
    flush ->
    	master([]);
    	
	print ->
	    io:format("~n~n========== MASTER NODE =========~n~nNodes: ~p~n~n", [Nodes]),
	    master(Nodes);
	
	{rpc, Function} ->
		io:format("RPC on master. "),
		Function(),
		master(Nodes);
	
	{network_graph, Filename} ->
		graphml:networkGraph(Nodes, Filename++".graphml"),
		master(Nodes);
	
	{spanning_graph, Filename} ->
		graphml:networkSpanningGraph(Nodes, Filename++".graphml"),
		master(Nodes)
    end.

spawnNode(NodeName) ->
	?LOCAL_NAME ! {spawnNode, NodeName}.

%compass() ->
%	io:format("Test running. "),
%	master!{rpc, fun() -> monSend(mute) end},
%	spawnNode(north),
%	spawnNode(east),
%	spawnNode(south),
%	timer:sleep(1000),
%	master!{rpc, fun() -> monSend(unmute) end},
%	compassTest(),
%	%master!network_graph,
%	done.
%
%compassTest() ->
%	node:store(node1,"Sweden","Stockholm").
%	%node:store(east,"Japan","Tokyo"),
%	%node:store(south,"Egypt","Kairo"),
%	%timer:sleep(500).
%	%node:findValue(north,"Japan").
%	%node:findNode(north,"east").

test() ->
	N = 1000,
	io:format("Test running. Spawning ~p nodes. ",[N]),
	spawnN(N),
	%master!{spanning_graph,"spanningGraph"},
	%node:store(node0,"Schweden","Helsinki"),
	%node:findValue(node0,"Schweden"),
	%storeCapitals(20),
	master!{network_graph,"topology"},
	master!{spanning_graph,"spanning"},
	timer:sleep(500),
	%io:format("~n~nPerforming test look up for key \"Angola\".~n"),
	%node:findValue(node0,"Angola"),
	test_done.
	%master!{rpc, fun() -> monSend({suppress,?FIND_NODE_PROCEDURE}) end},
	%master!{rpc, fun() -> monSend({allow,?FIND_NODE_PROCEDURE}) end},
	
	%master!{rpc, fun() -> node:findNode(node0,"node1") end},
	%timer:sleep(1000),

spawnN(0) -> 
	io:format("Spawn done.~n"),
	done;
spawnN(N) ->
	spawnNode(erlang:list_to_atom("node"++erlang:integer_to_list(N))),
	timer:sleep(20),
	spawnN(N-1).

muteFind() -> monSend({suppress,?FIND_NODE_PROCEDURE}).
monSend(Message) -> global:send(kademliaMonitor,Message).

storeCapitals(N) ->
	SendZero = fun({Country, City}) -> 
					%io:format("Request store ~p/~p",[Country,City]),
					node:store(node0,Country,City), 
					timer:sleep(300) 
			   end,
	io:format("~nPopulating the DHT. Storing ~p entries.",[N]),
	lists:foreach(SendZero, element(1, lists:split(N,capitals()))).

capitals() -> [
	{"Abkhazia","Sukhumi"},
	{"Afghanistan","Kabul"},
	{"Akrotiri and Dhekelia","Episkopi"},
	{"Albania","Tirana"},
	{"Algeria","Algiers"},
	{"American Samoa","Pago"},
	{"Andorra","Andorra"},
	{"Angola","Luanda"},
	{"Argentina","Buenos Aires2"},
	{"Armenia","Yerevan"},
	{"Aruba","Oranjestad"},
	{"Ascension Island","Georgetown"},
	{"Australia","Canberra"},
	{"Austria","Vienna"},
	{"Azerbaijan","Baku"},
	{"Bahamas","Nassau"},
	{"Bahrain","Manama"},
	{"Bangladesh","Dhaka"},
	{"Barbados","Bridgetown"},
	{"Belarus","Minsk"},
	{"Belgium","Brussels"},
	{"Belize","Belmopan"},
	{"Benin ","Porto"},
	{"Bermuda","Hamilton"},
	{"Bhutan","Thimphu"},
	{"Bolivia ","La"},
	{"Bolivia","Sucre"},
	{"Bosnia and Herzegovina","Sarajevo"},
	{"Botswana","Gaborone"},
	{"Brazil","Brasília"},
	{"British Virgin Islands","Road"},
	{"Brunei ","Bandar"},
	{"Bulgaria","Sofia"},
	{"Burkina Faso","Ouagadougou"},
	{"Burundi","Bujumbura"},
	{"Cambodia","Phnom"},
	{"Cameroon","Yaoundé"},
	{"Canada","Ottawa"},
	{"Cape Verde","Praia"},
	{"Cayman Islands","George"},
	{"Central African Republic","Bangui"},
	{"Chad","N"},
	{"Chile","Santiago"},
	{"China ","Beijing"},
	{"Christmas Island","Flying"},
	{"Cocos  Islands","West"},
	{"Colombia","Bogotá"},
	{"Comoros","Moroni"},
	{"Cook Islands","Avarua"},
	{"Costa Rica","San"},
	{"Côte d'Ivoire ","Yamoussoukro"},
	{"Croatia","Zagreb"},
	{"Cuba","Havana"},
	{"Curaçao","Willemstad"},
	{"Cyprus","Nicosia"},
	{"Czech Republic","Prague"},
	{"Democratic Republic of the Congo","Kinshasa"},
	{"Denmark","Copenhagen"},
	{"Djibouti","Djibouti"},
	{"Dominica","Roseau"},
	{"Dominican Republic","Santo"},
	{"East Timor","Dili"},
	{"Easter Island","Hanga"},
	{"Ecuador","Quito"},
	{"Egypt","Cairo"},
	{"El Salvador","San"},
	{"Equatorial Guinea","Malabo"},
	{"Eritrea","Asmara"},
	{"Estonia","Tallinn"},
	{"Ethiopia","Addis"},
	{"Falkland Islands","Stanley"},
	{"Faroe Islands","Tórshavn"},
	{"Federated States of Micronesia","Palikir"},
	{"Fiji","Suva"},
	{"Finland","Helsinki"},
	{"France","Paris"},
	{"French Guiana","Cayenne"},
	{"French Polynesia","Papeete"},
	{"Gabon","Libreville"},
	{"Gambia","Banjul"},
	{"Georgia","Tbilisi"},
	{"Germany","Berlin"},
	{"Ghana","Accra"},
	{"Gibraltar","Gibraltar"},
	{"Greece","Athens"},
	{"Greenland","Nuuk"},
	{"Guam","Hagåtña"},
	{"Guatemala","Guatemala"},
	{"Guinea-Bissau","Bissau"},
	{"Guinea","Conakry"},
	{"Guyana","Georgetown"},
	{"Haiti","Port"},
	{"Honduras","Tegucigalpa"},
	{"Hungary","Budapest"},
	{"Iceland","Reykjavík"},
	{"India","New"},
	{"Indonesia","Jakarta"},
	{"Iran","Tehran"},
	{"Iraq","Baghdad"},
	{"Ireland","Dublin"},
	{"Isle of Man","Douglas"},
	{"Israel ","Jerusalem"},
	{"Italy","Rome"},
	{"Jamaica","Kingston"},
	{"Japan","Tokyo"},
	{"Jordan","Amman"},
	{"Kazakhstan","Astana"},
	{"Kenya","Nairobi"},
	{"Kiribati","Tarawa"},
	{"Kosovo","Pristina"},
	{"Kuwait","Kuwait"},
	{"Kyrgyzstan","Bishkek"},
	{"Laos","Vientiane"},
	{"Latvia","Riga"},
	{"Lebanon","Beirut"},
	{"Lesotho","Maseru"},
	{"Liberia","Monrovia"},
	{"Libya","Tripoli"},
	{"Liechtenstein","Vaduz"},
	{"Lithuania","Vilnius"},
	{"Luxembourg","Luxembourg"},
	{"Macedonia","Skopje"},
	{"Madagascar","Antananarivo"},
	{"Malawi","Lilongwe"},
	{"Malaysia ","Kuala"},
	{"Maldives","Malé"},
	{"Mali","Bamako"},
	{"Malta","Valletta"},
	{"Marshall Islands","Majuro"},
	{"Mauritania","Nouakchott"},
	{"Mauritius","Port"},
	{"Mayotte","Mamoudzou"},
	{"Mexico","Mexico"},
	{"Moldova","Chisinau"},
	{"Monaco","Monaco"},
	{"Mongolia","Ulaanbaatar"},
	{"Montenegro","Podgorica"},
	{"Montserrat","Plymouth"},
	{"Morocco","Rabat"},
	{"Mozambique","Maputo"},
	{"Myanmar","Naypyidaw"},
	{"Nagorno-Karabakh Republic","Stepanakert"},
	{"Namibia","Windhoek"},
	{"Nauru ","Yaren"},
	{"Nepal","Kathmandu"},
	{"Netherlands ","Amsterdam"},
	{"New Caledonia","Nouméa"},
	{"New Zealand","Wellington"},
	{"Nicaragua","Managua"},
	{"Niger","Niamey"},
	{"Nigeria","Abuja"},
	{"Niue","Alofi"},
	{"Norfolk Island","Kingston"},
	{"North Korea","Pyongyang"},
	{"Northern Cyprus","Nicosia"},
	{"Northern Mariana Islands","Saipan"},
	{"Norway","Oslo"},
	{"Oman","Muscat"},
	{"Pakistan","Islamabad"},
	{"Palau","Ngerulmud"},
	{"Palestine ","Jerusalem"},
	{"Panama","Panama"},
	{"Papua New Guinea","Port"},
	{"Paraguay","Asunción"},
	{"Peru","Lima"},
	{"Philippines","Manila"},
	{"Pitcairn Islands","Adamstown"},
	{"Poland","Warsaw"},
	{"Portugal","Lisbon"},
	{"Puerto Rico","San"},
	{"Qatar","Doha"},
	{"Republic of China ","Taipei"},
	{"Republic of the Congo","Brazzaville"},
	{"Romania","Bucharest"},
	{"Russia","Moscow"},
	{"Rwanda","Kigali"},
	{"Saint Barthélemy","Gustavia"},
	{"Saint Helena","Jamestown"},
	{"Saint Kitts and Nevis","Basseterre"},
	{"Saint Lucia","Castries"},
	{"Saint Martin","Marigot"},
	{"Saint Vincent and the Grenadines","Kingstown"},
	{"Samoa","Apia"},
	{"San Marino","San"},
	{"São Tomé and Príncipe","São"},
	{"Saudi Arabia","Riyadh"},
	{"Scotland","Edinburgh"},
	{"Senegal","Dakar"},
	{"Serbia","Belgrade"},
	{"Seychelles","Victoria"},
	{"Sierra Leone","Freetown"},
	{"Singapore","Singapore"},
	{"Sint Maarten","Philipsburg"},
	{"Slovakia","Bratislava"},
	{"Slovenia","Ljubljana"},
	{"Solomon Islands","Honiara"},
	{"Somalia","Mogadishu"},
	{"Somaliland ","Hargeisa"},
	{"South Africa ","Pretoria"},
	{"South Georgia and the South Sandwich Islands","Grytviken"},
	{"South Korea","Seoul"},
	{"South Ossetia","Tskhinvali"},
	{"South Sudan South Sudan","Juba"},
	{"Spain","Madrid"},
	{"Sudan","Khartoum"},
	{"Suriname","Paramaribo"},
	{"Swaziland ","Mbabane"},
	{"Sweden","Stockholm"},
	{"Switzerland","Bern"},
	{"Syria","Damascus"},
	{"Tajikistan","Dushanbe"},
	{"Tanzania ","Dodoma"},
	{"Thailand","Bangkok"},
	{"Togo","Lomé"},
	{"Tonga","Nuku"},
	{"Transnistria","Tiraspol"},
	{"Trinidad and Tobago","Port"},
	{"Tristan da Cunha","Edinburgh"},
	{"Tunisia","Tunis"},
	{"Turkey","Ankara"},
	{"Turkmenistan","Ashgabat"},
	{"Turks and Caicos Islands","Cockburn"},
	{"Tuvalu","Funafuti"},
	{"Uganda","Kampala"},
	{"Ukraine","Kiev"},
	{"United Arab Emirates","Abu"},
	{"United Kingdom Northern Ireland","Belfast"},
	{"United Kingdom;  England","London"},
	{"United States Virgin Islands","Charlotte"},
	{"United States","Washington"},
	{"Uruguay","Montevideo"},
	{"Uzbekistan","Tashkent"},
	{"Vanuatu","Port"},
	{"Vatican City ","Vatican"},
	{"Venezuela","Caracas"},
	{"Vietnam","Hanoi"},
	{"Wales","Cardiff"},
	{"Wallis and Futuna","Mata"},
	{"Western Sahara ","El"},
	{"Yemen","Sanaá"},
	{"Zambia","Lusaka"},
	{"Zimbabwe","Harare"}
].
