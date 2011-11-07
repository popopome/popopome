-module(xml2json_test).

%% TEST
-export([test_animal/0,
	 test_review_rss/0,
	 test_add_empty/0,
	 test_strange_el/0]).

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
    {ok, Json} = xml2json:xmlstring2json(?ANIMAL_XML_TEST_STRING),
    io:fwrite("~p~n", [Json]).


test_review_rss()->
    xml2json:xmlfile2json("F:/prjs/github/erlang/xml2json/src/xml2json_sample.xml").

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
    xml2json:xmlelement2json(EmptyNode).

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
    xml2json:xmlelement2json(XmlElement).






