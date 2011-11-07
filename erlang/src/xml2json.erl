-module(xml2json).

%% API
-export([xmlstring2json/1,
	 xmlfile2json/1]).

%% TEST
-export([test_animal/0,
	 test_review_rss/0,
	 test_add_empty/0,
	 test_strange_el/0]).

-include_lib("xmerl/include/xmerl.hrl").

-define(TEXTNODE, 'ERLANG-text-NODE').
-define(DQUOTE, "\"").


%% ============================================================
%% API
%% ============================================================
xmlstring2json(Xml)->
    {Root, _Rest} = xmerl_scan:string(Xml),
    xml2json(Root).

xmlfile2json(FilePath)->
    {Root, _Rest} = xmerl_scan:file(FilePath),
    xml2json(Root).



%% ============================================================
%% Internal Functions
%% ============================================================
atom_to_json_key(Atom) when is_atom(Atom)->
    [?DQUOTE, atom_to_binary(Atom, utf8), ?DQUOTE].


store_child_to_dict(Name, Value, Dict)->
    case dict:find(Name, Dict) of
	{ok, List}->
	    NewList = List ++ [Value],
	    dict:store(Name, NewList, Dict);
	_ ->
	    dict:store(Name, [Value], Dict)
    end.

xml2json(Root)->
    {K,V} = xmlelement2json(Root),
    lists:flatten(["{", atom_to_json_key(K), ":", V, "}"]).


%%------------------------------------------------------------
%% @doc
%% Convert xml element to json string
%% @spec xmlelement2json(_XmlElement::xmlElement)->
%%       {atom(), dict()}
%% @end
%%------------------------------------------------------------
xmlelement2json(#xmlElement{name=Name,
			    attributes=Attributes,
			    content=Content})->
    ChildMap = lists:foldl(fun xmlelement_grouping_func/2,
			   dict:new(),
			   Content),
    FilteredMap = add_empty_string_if_no_child(
		    erase_textnode_if_necessary(ChildMap)),
    %% Xml attribute can cause some problem
    %% I dropped the code.
    %% AttrAddedMap = attributes_to_child_map(Attributes, FilteredMap),
    {Name, childmap2json(FilteredMap)};

xmlelement2json(#xmlText{value=ValueRaw})->
    Value = trim:trim(ValueRaw),
    try
	{?TEXTNODE, integer_to_list(list_to_integer(Value))}
    catch
	error:badarg->
	    {?TEXTNODE, list_to_binary([$", Value, $"])}
    end.


%%
%% Grouping XML element to dictionary based storage
%%
xmlelement_grouping_func(#xmlText{value=Value}=El,Dict)->
    case dict:find(?TEXTNODE, Dict) of
	{ok, _}->Dict;
	_->
	    TrimValue = trim:trim(Value),
	    dict:store(?TEXTNODE, ["\"", TrimValue, "\""], Dict)
    end;
xmlelement_grouping_func(El,Dict)->
    {Name, Json} = xmlelement2json(El),
    store_child_to_dict(Name, Json, Dict).





%%
%% Remove text node if Dict holds
%% several types of element.
%%
%% <A>
%%   <TextNode/>
%%   <MeaningfulNode/>
%% </A>
%% In upper case,
%% I ignore TextNode, cause it has no
%% valuable things
%%
erase_textnode_if_necessary(Dict)->
    case dict:size(Dict) of
	1->
	    Dict;
	_->
	    case dict:find(?TEXTNODE, Dict) of
		{ok, Value}->
		    dict:erase(?TEXTNODE, Dict);
		_ -> Dict
	    end
    end.


%%
%% Add empty string if there is no child
%%
add_empty_string_if_no_child(ChildMap)->
    case dict:size(ChildMap) of
	0->
	    dict:store(?TEXTNODE, ["\"\""], ChildMap);
	_ -> ChildMap
    end.

%%------------------------------------------------------------
%% @doc
%% Convert attribute value of XmlElement to Json string
%% @spec attribute_value_to_json(Value :: integer() | atom() | IOList())
%%       -> string()
%% @end
%%------------------------------------------------------------
attribute_value_to_json(Value) when is_integer(Value)->
    integer_to_list(Value);
attribute_value_to_json(Value) when is_atom(Value)->
    atom_to_json_key(Value);
attribute_value_to_json(Value)->
    [?DQUOTE, Value, ?DQUOTE].


%%------------------------------------------------------------
%% @doc
%% Convert attributes to child map
%%
%% @spec attributes_to_child_map(xmlAttributes())->
%%       dict().
%%
%% @end
%%------------------------------------------------------------
attributes_to_child_map(AttrList, Map)->
    Fun=fun (#xmlAttribute{name=Name, value=Value},
	     Dict)->
		Json = attribute_value_to_json(Value),
		store_child_to_dict(Name, Json, Dict)
	end,
    lists:foldl(Fun,
		Map,
		AttrList).


%%
%% child2json_func
%% 
child2json_func({?TEXTNODE, TextList}, {List,_})->
    {[TextList|List], false};
child2json_func({Name, List}, {OldList,_})->
    case length(List)>1 of
	true-> ArrayList = string:join(List, ","),
	       Formatted = io_lib:format("\"~s\":[~s]",
					 [Name, ArrayList]),
	       {[Formatted|OldList], true};
	false->
	    Formatted = io_lib:format("\"~s\":~s",
				      [Name,List]),
	    {[Formatted|OldList], true}
    end.

%%
%% Convert given child map to json strings
%%
childmap2json(Dict)->
    L = dict:to_list(Dict),
    {L1,JsonMapFormatUsed} = lists:foldl(fun child2json_func/2,
					 {[],true},
					 L),
    ReverseList = lists:reverse(L1),
    L2 = string:join(ReverseList, ","),
    case JsonMapFormatUsed of
	true->lists:flatten([${, L2, $}]);
	false-> lists:flatten(L2)
    end.


%% ============================================================
%% TEST
%% ============================================================

-define(ANIMAL_XML_TEST_STRING,
	"<animal kind=\"pet\"><dog>abc</dog><dog>bbc</dog><cat>tom</cat>
<height>130</height>
<a:entry>
 <a:updated>2011-10-11T13:15:01.533Z</a:updated>
 <a:title type=\"text\"></a:title>
 <a:id>
   tag:catalog.zune.net,2011-11-04:/apps/47ae03f0-99d1-df11-9eae-00237de2db9e/reviews
 </a:id>
<a:content type=\"html\">
Seriously, no MANGO UPDATE!?!!??...where are the updates...this is terrible compared to the iPhone or Android app...why are you not updating app...PLEASE UPDATE
</a:content>
<a:author>
<a:name>ant1906</a:name>
</a:author>
<userRating>2</userRating>
</a:entry>

<a:title type=\"text\">List Of Items</a:title></animal>").

test_animal()->
    {Root, _} = xmerl_scan:string(?ANIMAL_XML_TEST_STRING),
    JSON = xml2json(Root),
    io:fwrite("~s~n", [JSON]).


test_review_rss()->
    {Node, Rest} = xmerl_scan:file("F:/prjs/github/erlang/src/xml2json_sample.xml"),
    JSON = xml2json(Node),
    io:fwrite("~s~n", [JSON]).


test_add_empty()->
    EmptyNode = {xmlElement,
		 'a:title',
		 'a:title',
		 {"a","title"},
		 {xmlNamespace,[],[]},
		 [{'a:entry',7},{animaal,1}],
		 4,
		 [{xmlAttribute,type,[],[],[],[],1,[],"text",false}],
		 [],[],undefined,undeclared},
    xmlelement2json(EmptyNode).

test_strange_el()->
    XmlElement = {xmlElement,'a:author','a:author',
		  {"a","author"},
		  {xmlNamespace,[],[]},
		  [{'a:entry',7},{animal,1}],
		  10,[],
		  [{xmlText,
		    [{'a:author',10},{'a:entry',7},{animal,1}],
		    1,[],"\n",text},
		   {xmlElement,'a:name','a:name',
		    {"a","name"},
		    {xmlNamespace,[],[]},
		    [{'a:author',10},{'a:entry',7},{animal,1}],
		    2,[],
		    [{xmlText,
		      [{'a:name',2},
		       {'a:author',10},
		       {'a:entry',7},
		       {animal,1}],
		      1,[],"ant1906",text}],
		    [],undefined,undeclared},
		   {xmlText,
		    [{'a:author',10},{'a:entry',7},{animal,1}],
		    3,[],"\n",text}],
		  [],undefined,undeclared},
    xmlelement2json(XmlElement).
