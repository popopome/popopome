-module(xml2json).

%%------------------------------------------------------------
%% API
%%------------------------------------------------------------
-export([xmlstring2json/1,
	 xmlfile2json/1]).

%%------------------------------------------------------------
%% For testing
%%------------------------------------------------------------
-export([xmlelement2json/1]).


-include_lib("xmerl/include/xmerl.hrl").

-define(TEXTNODE, 'ERLANG-text-NODE').
-define(DQUOTE, "\"").


%%%============================================================
%%%  API
%%%============================================================
xmlstring2json(Xml)->
    {Root, _Rest} = xmerl_scan:string(Xml),
    xml2json(Root).

xmlfile2json(FilePath)->
    {Root, _Rest} = xmerl_scan:file(FilePath),
    xml2json(Root).


%%%============================================================
%%% Internal Functions
%%%============================================================

%%------------------------------------------------------------
%% @doc
%% Convert root node to json string
%% @spec xml2json(Root::xmlElement())->
%%       {ok, binary()} | exception
%% @end
%%------------------------------------------------------------
xml2json(Root)->
    {K,V} = xmlelement2json(Root),
    {ok, list_to_binary(["{", atom_to_json_key(K), ":", V, "}"])}.


%%------------------------------------------------------------
%% @doc
%% Convert xml element to json string
%% @spec xmlelement2json(_XmlElement::xmlElement)->
%%       {atom(), dict()}
%% @end
%%------------------------------------------------------------
xmlelement2json(#xmlElement{name=Name,
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


%%------------------------------------------------------------
%% @doc
%% Grouping XML element to dictionary based storage
%% @end
%%------------------------------------------------------------
xmlelement_grouping_func(#xmlText{value=Value},Dict)->
    case dict:find(?TEXTNODE, Dict) of
	{ok, _}->Dict;
	_->
	    TrimValue = trim:trim(Value),
	    dict:store(?TEXTNODE, ["\"", TrimValue, "\""], Dict)
    end;
xmlelement_grouping_func(El,Dict)->
    {Name, Json} = xmlelement2json(El),
    store_child_to_dict(Name, Json, Dict).





%%------------------------------------------------------------
%% @doc
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
%% @end
%%------------------------------------------------------------
erase_textnode_if_necessary(Dict)->
    case dict:size(Dict) of
	1->
	    Dict;
	_->
	    case dict:find(?TEXTNODE, Dict) of
		{ok, _Value}->
		    dict:erase(?TEXTNODE, Dict);
		_ -> Dict
	    end
    end.


%%------------------------------------------------------------
%% @doc
%% Add empty string if there is no child
%% @end
%%------------------------------------------------------------
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
-ifdef(USE_ATTRIBUTE).
attribute_value_to_json(Value) when is_integer(Value)->
    integer_to_list(Value);
attribute_value_to_json(Value) when is_atom(Value)->
    atom_to_json_key(Value);
attribute_value_to_json(Value)->
    [?DQUOTE, Value, ?DQUOTE].
-endif.


%%------------------------------------------------------------
%% @doc
%% Convert attributes to child map
%%
%% @spec attributes_to_child_map(xmlAttributes())->
%%       dict().
%%
%% @end
%%------------------------------------------------------------
-ifdef(USE_ATTRIBUTE).
attributes_to_child_map(AttrList, Map)->
    Fun=fun (#xmlAttribute{name=Name, value=Value},
	     Dict)->
		Json = attribute_value_to_json(Value),
		store_child_to_dict(Name, Json, Dict)
	end,
    lists:foldl(Fun,
		Map,
		AttrList).
-endif.


%%------------------------------------------------------------
%% @doc
%% child2json_func
%% @end
%%------------------------------------------------------------
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

%%------------------------------------------------------------
%% @doc
%% Convert given child map to json strings
%% @end
%%------------------------------------------------------------
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












