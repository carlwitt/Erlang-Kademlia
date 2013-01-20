-module(graphml).

-export([
			networkGraph/2
		]).
-include("include/node.hrl").
-include("include/metric.hrl").
-include("include/routingTable.hrl").

-define(OUT_DIR,"./graphs/").

-compile(export_all).

%% =================================================================================================
%% Value distributions
%% =================================================================================================

% plots the appearance of data belonging to a key in the graph.
dataGraph(Nodes, _Key, Filename) ->
	io:format("~n----Data Graph saving----~n"),
	io:format("Retrieving data.~n"),
	Tokens = [ requestNodeData(Node) || Node <- Nodes],
    RoutingInfo = lists:zip(Nodes, [ receiveNodeData(Token) || Token <- Tokens ]),
    NodeXML = lists:flatten(lists:map(fun(X)->xmlNode(X) end,Nodes)),
    EdgesForOne = 	fun({From,Tos}) -> 
    					EdgeForOne = 	fun(To) ->
    										ID = erlang:atom_to_list(From#peer.name)++erlang:atom_to_list(To#peer.name),
    										Dist = metric:distance(From#peer.key,To#peer.key),
    										xmlEdge(ID,From,To,Dist)
    									end,
    					lists:concat(lists:map(EdgeForOne,Tos)) 
    				end,
    EdgeXML = lists:concat(lists:map(EdgesForOne, RoutingInfo)),
    io:format("~nWriting network structure as ~s: ~p~n",[?OUT_DIR++Filename,
    	file:write_file(?OUT_DIR++Filename,xmlHeader() ++ NodeXML ++ EdgeXML ++ xmlFooter())]).
   
requestNodeData(#peer{pid = PID}) -> 
    Token =  make_ref(),
    PID ! {get_data, self(), Token},
    Token.
   
receiveNodeData(Token) ->
	receive {Data, Token} -> Data end.
	
%% =================================================================================================
%% Routing table/tree plot
%% =================================================================================================

routingTableGraph(#routingData{root = Root}, Filename) ->
	{_,XML} = routingTree(Root,""),
	io:format("~nWriting routing table as ~s: ~p~n",[?OUT_DIR++Filename, 
			file:write_file(?OUT_DIR++Filename,xmlHeader() ++ XML ++ xmlFooter())]).
    

routingTree( #leaf{bucket = Bucket}, Prefix ) ->
	Strip = fun(RE) -> RE#routingEntry.peer end,
	
	%{_, [PrefixValue],_} = io_lib:fread("~2u", Prefix),
	PrefixValue = 0,
	BucketNode = #peer{
						name=erlang:list_to_atom(Prefix), 
						key = #key{hash=PrefixValue, readable=Prefix},
						spawnOrder = -1
					  },
	BucketNodeXML = xmlNode(BucketNode),
	NodeXML = lists:flatten(lists:map(	fun(X)->xmlNode(X) end,
										lists:map(Strip,Bucket)
									)
								),
	ConnectWithBucket = fun(To) ->
							From = BucketNode,
							ID = erlang:atom_to_list(From#peer.name)++erlang:atom_to_list(To#peer.name),
							Dist = metric:distance(From#peer.key,To#peer.key),
							xmlEdge(ID, From, To, Dist)
						end,
	EdgeXML = lists:flatten(lists:map(	ConnectWithBucket,
										lists:map(Strip,Bucket)
									)
								),
	{BucketNode, BucketNodeXML++EdgeXML++NodeXML};

routingTree( #innerNode{zero = Zero, one = One}, Prefix ) ->
	{ZeroNode, ZeroXML} = routingTree(Zero, Prefix++"0"),
	{OneNode, OneXML} = routingTree(One, Prefix++"1"),

	%{_, [PrefixValue],_} = io_lib:fread("~2u", Prefix),
	PrefixValue = 0,
	InnerNode = #peer{
						name=erlang:list_to_atom(Prefix), 
						key = #key{hash=PrefixValue, readable=Prefix},
						spawnOrder = -1
					  },
	InnerNodeXML = xmlNode(InnerNode),
	ConnectWithNode = fun(To) ->
							From = InnerNode,
							ID = erlang:atom_to_list(From#peer.name)++erlang:atom_to_list(To#peer.name),
							Dist = metric:distance(From#peer.key,To#peer.key),
							xmlEdge(ID, From, To, Dist)
						end,
	EdgeXML = ConnectWithNode(ZeroNode)++ConnectWithNode(OneNode),
	{InnerNode, InnerNodeXML++EdgeXML++ZeroXML++OneXML}.
	
%% =================================================================================================
%% Network topology plotting
%% =================================================================================================
    
% takes a list of nodes, retrieves their routing tables and build a yed/graphml version of this structure 
networkGraph(Nodes, Filename) ->
	io:format("~n----Graph saving----~n"),
	
	RequestRoutingTable = fun(#peer{pid = ThePID}) -> ThePID ! {get_routing_table, self()} end,
	ReceiveRoutingTable = fun(Accum) -> receiveRoutingTable(Accum) end,
	RoutingOrder = fun({#peer{spawnOrder=SO1},_},{#peer{spawnOrder=SO2},_}) ->
			if SO1 =< SO2 -> true;
			true ->	false
			end
		end,
	DataComing = fun(Result)-> (not erlang:is_list(Result))  end,
	
	io:format("Requesting routing tables.~n"),
	lists:foreach(RequestRoutingTable, Nodes),
	io:format("Retrieving and sorting data.~n"),
	RoutingInfo = lists:sort(
								RoutingOrder,
								while(DataComing, ReceiveRoutingTable, [])
							 ),
	io:format("~nCreating Nodes. "),
    NodeXML = lists:flatten(lists:map(fun(X)->xmlNode(X) end,Nodes)),
    EdgesForOne = 	fun({From,Tos}) -> 
    					EdgeForOne = 	fun(To) ->
    										ID = erlang:atom_to_list(From#peer.name)++erlang:atom_to_list(To#peer.name),
    										Dist = metric:distance(From#peer.key,To#peer.key),
    										xmlEdge(ID,From,To,Dist)
    									end,
    					lists:concat(lists:map(EdgeForOne,Tos)) 
    				end,
    io:format("Creating Edges.~n"),
    EdgeXML = lists:concat(lists:map(EdgesForOne, RoutingInfo)),
    io:format("Writing network structure as ~s: ~p~n",[?OUT_DIR++Filename, 
    	file:write_file(?OUT_DIR++Filename,xmlHeader() ++ NodeXML ++ EdgeXML ++ xmlFooter())]).


% takes a list of nodes, retrieves their routing tables and build a yed/graphml version of this structure 
networkSpanningGraph(Nodes, Filename) ->
	
	io:format("~n----Graph saving----~n"),
	io:format("Retrieving routing tables.~n"),
	
	RequestRoutingTable = fun(#peer{pid = ThePID}) -> ThePID ! {get_routing_table, self()} end,
	ReceiveRoutingTable = fun(Accum) -> receiveRoutingTable(Accum) end,
	RoutingOrder = fun({#peer{spawnOrder=SO1},_},{#peer{spawnOrder=SO2},_}) ->
			if SO1 =< SO2 -> true;
			true ->	false
			end
		end,
	DataComing = fun(Result)-> (not erlang:is_list(Result))  end,
	
	lists:foreach(RequestRoutingTable, Nodes),
	RoutingInfo = lists:sort(
								RoutingOrder,
								while(DataComing, ReceiveRoutingTable, [])
							 ),
	
	%io:format("~p~n",[RoutingInfo]),
    NodeXML = lists:flatten(lists:map(fun(X)->xmlNode(X) end,Nodes)),
    ForwardEdgesForOne = 	fun({From,Tos}, {XML, TreatedNodes}) -> 
    						%io:format("Edges for ~p continue ~p ~p ~n",[From,XML,TreatedNodes]),
    						EdgeForOne = 	fun(To,{X,N}) ->
												NodeAlreadySpanned = lists:member(To, N),
    											if NodeAlreadySpanned ->
    												%io:format("~s: already spanned in ~p~n",[kademliaGlobal:senderFormat(To),N]),
    												{X,N};
    											true ->
    												%io:format("~s~n",[kademliaGlobal:senderFormat(To)]),
	    											ID = erlang:atom_to_list(From#peer.name)++erlang:atom_to_list(To#peer.name),
    												Dist = metric:distance(From#peer.key,To#peer.key),
    												{X++xmlEdge(ID,From,To,Dist),N++[To]}
    											end
    										end,
	    					lists:foldl(EdgeForOne, {XML, TreatedNodes}, Tos) 
    				end,
    {EdgeXML,_} = lists:foldl(ForwardEdgesForOne, {"",[]}, RoutingInfo),
    io:format("~nWriting network spanning structure as ~s: ~p~n",
    			[?OUT_DIR++Filename, 
		    	file:write_file(
    						?OUT_DIR++Filename,
    						xmlHeader() ++ NodeXML ++ EdgeXML ++ xmlFooter()
    						)
    				]).

while(Predicate, Function, InitialValue) ->
	io:format("."),
	Result = Function(InitialValue),
	Terminate = Predicate(Result),
	if Terminate ->
		%io:format("Terminate while."),
		InitialValue;
	true ->
		while(Predicate, Function, Result)
	end.
  
requestRoutingTable(#peer{pid = PID}) -> 
    PID ! {get_routing_table, self()}.

receiveRoutingTable(Accum) ->
	Strip = fun(RE) -> RE#routingEntry.peer end,
	%io:format("receive "),
	receive 
		{routing_table, Peer, Entries} -> 
			Accum++[{Peer, lists:map(Strip,Entries)}]
	after 1100 ->
		false
	end.
%% =================================================================================================
%% graphml building blocks
%% =================================================================================================

xmlLabel(String) ->
	io_lib:format("<y:NodeLabel alignment=\"center\" autoSizePolicy=\"content\" fontFamily=\"Dialog\" fontSize=\"12\" fontStyle=\"plain\" hasBackgroundColor=\"false\" hasLineColor=\"false\" height=\"18.1328125\" modelName=\"internal\" modelPosition=\"c\" textColor=\"#000000\" visible=\"true\" width=\"26.763671875\" x=\"1.6181640625\" y=\"5.93359375\"> ~s </y:NodeLabel>",[String]).

xmlNode(#peer{name = Name, key = #key{hash = Hash, readable = Readable}, spawnOrder=SpawnOrder}) ->
"	<node id=\""++ erlang:atom_to_list(Name) ++"\">\n"++
"	 <data key=\"spawnOrder\">"++ erlang:integer_to_list(SpawnOrder) ++"</data>"++
"	<data key=\"d6\">\n"++
"        <y:ShapeNode>\n"++
"          <y:Geometry height=\"72.0\" width=\"72.0\" x=\"687.0745426789618\" y=\"509.876601499971\"/>\n"++
"          <y:Fill color=\"#CCCCFF\" transparent=\"false\"/>\n"++
"          <y:BorderStyle color=\"#000000\" type=\"line\" width=\"1.0\"/>\n"++
"          <y:Shape type=\"ellipse\"/>\n"++
"\t" ++ xmlLabel(io_lib:format("~p~n~p",[utils:decInt2BinStr(Hash),Readable]))++"\n"++
"        </y:ShapeNode>\n"++
"      </data>\n"++
"    </node>".

xmlEdge(ID, From, To, Dist) ->
"    <edge id=\""++ ID ++"\" source=\""++ erlang:atom_to_list(From#peer.name) ++"\" target=\""++ erlang:atom_to_list(To#peer.name) ++"\">\n"++
"	 <data key=\"dist\">"++ erlang:integer_to_list(Dist) ++"</data>"++
"      <data key=\"d9\">\n"++
"        <y:PolyLineEdge>\n"++
"          <y:Path sx=\"0.0\" sy=\"0.0\" tx=\"0.0\" ty=\"0.0\"/>\n"++
"          <y:LineStyle color=\"#000000\" type=\"line\" width=\"1.0\"/>\n"++
"          <y:Arrows source=\"none\" target=\"standard\"/>\n"++
"          <y:BendStyle smoothed=\"false\"/>\n"++
"        </y:PolyLineEdge>\n"++
"      </data>\n"++
"    </edge>".

xmlHeader() ->
"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"++
"<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:y=\"http://www.yworks.com/xml/graphml\" xmlns:yed=\"http://www.yworks.com/xml/yed/3\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd\">\n"++
"  <!--Created by yFiles for Java 2.8-->\n"++
"  <key for=\"graphml\" id=\"d0\" yfiles.type=\"resources\"/>\n"++
"  <key for=\"port\" id=\"d1\" yfiles.type=\"portgraphics\"/>\n"++
"  <key for=\"port\" id=\"d2\" yfiles.type=\"portgeometry\"/>\n"++
"  <key for=\"port\" id=\"d3\" yfiles.type=\"portuserdata\"/>\n"++
"  <key attr.name=\"url\" attr.type=\"string\" for=\"node\" id=\"d4\"/>\n"++
"  <key attr.name=\"description\" attr.type=\"string\" for=\"node\" id=\"d5\"/>\n"++
"  <key for=\"node\" id=\"d6\" yfiles.type=\"nodegraphics\"/>\n"++
"  <key attr.name=\"url\" attr.type=\"string\" for=\"edge\" id=\"d7\"/>\n"++
"  <key attr.name=\"description\" attr.type=\"string\" for=\"edge\" id=\"d8\"/>\n"++
"  <key for=\"edge\" id=\"d9\" yfiles.type=\"edgegraphics\"/>\n"++
"  <key attr.name=\"Order of creation (lowest first)\" attr.type=\"int\" for=\"node\" id=\"spawnOrder\">\n"++
"    <default>-1</default>\n"++
"  </key>"++
"  <key attr.name=\"Metric distances (xor of keys)\" attr.type=\"int\" for=\"edge\" id=\"dist\">\n"++
"    <default>-1</default>\n"++
"  </key>"++
"  <graph edgedefault=\"directed\" id=\"G\">\n".

xmlFooter() ->
"  </graph> <data key=\"d0\"> <y:Resources/> </data> </graphml>".



requestNodeInfo(#peer{pid = PID}) -> 
    Token =  make_ref(),
    PID ! {get_routing_table, self(), Token},
    Token.

receiveNodeInfo(Token) ->
	Strip = fun(RE) -> RE#routingEntry.peer end,
	io:format("."),
	receive 
		{routing_table, Table, Token} -> io:format("*"), lists:map(Strip,Table)
	end.

