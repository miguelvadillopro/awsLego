-module(marker_translator_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../src/constants.hrl").

%group_test() ->
%    Group = [{"marker", 170}, {0, [2,10]}, {1, [10,10]},{2,[2,2]},{3,[10,2]}],
%%    ?assertEqual(0, markerTranslator:togroup(Group)).

happy_path_markers_midpoint_calculation_test() ->
    Raw_data = [[{?Marker_key,170},{0,[8,2]},{1,[9,8]},{2,[3,9]},{3,[2,4]}],[{?Marker_key,123},{0,[6,1]},{1,[10,9]},{2,[4,10]},{3,[1,7]}]],
    Data_received = marker_translator:markers_midpoint_coords(Raw_data),
    ?assertNotEqual([],Data_received),
    ?assertEqual(length(Raw_data),length(Data_received)),
    ?assert(all_keys([?X0_key,?Y0_key,?X1_key,?Y1_key], Data_received )).

empty_list_markers_midpoint_calculation_test() ->
    Data_received = marker_translator:markers_midpoint_coords([]),
    ?assertEqual([], Data_received).

coords_rotated_same_midpoints_test() ->
    Data = [[{?Marker_key, 23},{0,[3,2]},{1,[2,3]},{2,[0,2]},{3,[1,1]}]],
    Rotated_data = [[{?Marker_key, 23},{0,[1,1]},{1,[3,2]},{2,[2,3]},{3,[0,2]}]],
    Coords = marker_translator:markers_midpoint_coords(Data),
    Rotated_coords = marker_translator:markers_midpoint_coords(Rotated_data),
    ?assertEqual(lists:sort(Coords), lists:sort(Rotated_coords)).

all_keys(Keys, Target_list) -> all_keys(Keys, Target_list, true).
all_keys(_, [], true) -> true;
all_keys(_, _, false) -> false;
all_keys(Keys, [Head|Tail], _) ->
    Coords = orddict:fetch(?Coords_key, orddict:from_list(Head)),
    all_keys(Keys, Tail, lists:sort(orddict:fetch_keys(Coords)) =:= lists:sort(Keys)).







classification_test() ->
%    Raw_data = [[{?Marker_key,5},
%                 {?Coords_key,[{?X0_key,2.5},{?X1_key,8.0},{?Y0_key,4.0},{?Y1_key,9.5}]}],
%                [{?Coords_key,[{?X0_key,2.5},{?X1_key,8.5},{?Y0_key,3.0},{?Y1_key,8.5}]},
%                 {?Marker_key,17}]],
    Raw_data = [[{?Marker_key, 5}],[{?Marker_key, 17}],[{?Marker_key, 23}]],
   
    Data_received = marker_translator:markers_classification(Raw_data),

    ?assert(lists:all(fun(X) -> (orddict:is_key(?Type_key, X)) end,Data_received)),
    ?assert(lists:any(fun(X) -> (orddict:is_key(?Component_key, X)) end,Data_received)),
    ?assertEqual(length(Raw_data),length(Data_received)),

    Element = lists:nth(rand:uniform(length(Data_received)), Data_received),
    Type = orddict:fetch(?Type_key, Element),

    ?assert(lists:member(Type, [?Group, ?Element, ?Connection])).

forming_groups_test() ->


    Raw_data = [[{?Marker_key,21},
                 {?Coords_key,[{?X0_key,4},{?X1_key,5},{?Y0_key,1},{?Y1_key,2}]},
                 {?Type_key,"Connection"}],
                [{?Component_key,vp},
                 {?Marker_key,5},
                 {?Coords_key,[{?X0_key,1},{?X1_key,6},{?Y0_key,3},{?Y1_key,6}]},
                 {?Type_key,"Group"}],
                [{?Component_key,s3},
                 {?Coords_key,[{?X0_key,2},{?X1_key,3},{?Y0_key,4},{?Y1_key,5}]},
                 {?Marker_key,17},
                 {?Type_key,"Element"}]],


    Data_received = marker_translator:grouping_elements(Raw_data),

    ?assertEqual(1, length(lists:filter(fun(X) -> orddict:is_key(?Groups_key, X) end, Data_received))).


forming_connections_test() ->

   Raw_data = [[{?Marker_key,21},
                {?Coords_key,[{?X0_key,3},{?X1_key,6},{?Y0_key,1},{?Y1_key,2}]},
                {?Type_key,?Connection}],
               [{?Component_key,?EC2},
                {?Marker_key,12},
                {?Coords_key,[{?X0_key,1},{?X1_key,2},{?Y0_key,1},{?Y1_key,2}]},
                {?Type_key,?Element}],
               [{?Component_key,?S3},
                {?Coords_key,[{?X0_key,7},{?X1_key,8},{?Y0_key,1},{?Y1_key,2}]},
                {?Marker_key,17},
                {?Type_key,?Element}],
               [{?Component_key,?VPC},
                {?Marker_key,5},
                {?Coords_key,[{?X0_key,1},{?X1_key,6},{?Y0_key,3},{?Y1_key,6}]},
                {?Type_key,?Group}]],


    Data_received = marker_translator:connecting_elements(Raw_data),
    Elements_with_connections = lists:filter(fun(X) -> orddict:is_key(?Groups_key, X) end, Data_received),
    [First_element|_] = Elements_with_connections,
    Connections_first_element = orddict:fetch(?Groups_key, orddict:from_list(First_element)),
    [First_connection|_] = Connections_first_element,


    ?assertEqual(1, length(Elements_with_connections)),
    ?assertEqual(1, length(Connections_first_element)),
    ?assertEqual(17, orddict:fetch(?Marker_key, orddict:from_list(First_connection))).


terraform_translation_test() ->

  Raw_data = [[{?Marker_key,21},
               {?Coords_key,[{?X0_key,3},{?X1_key,6},{?Y0_key,1},{?Y1_key,2}]},
               {?Type_key,?Connection}],
              [{?Component_key,?EC2},
               {?Groups_key,[[
                              {?Component_key,?S3},
                              {?Coords_key,[{?X0_key,7},{?X1_key,8},{?Y0_key,1},{?Y1_key,2}]},
                              {?Marker_key,17},
                              {?Type_key,?Element}]
                            ]},
               {?Marker_key,12},
               {?Coords_key,[{?X0_key,1},{?X1_key,2},{?Y0_key,1},{?Y1_key,2}]},
               {?Type_key,?Element}],
              [{?Component_key,?S3},
               {?Coords_key,[{?X0_key,7},{?X1_key,8},{?Y0_key,1},{?Y1_key,2}]},
               {?Marker_key,17},
               {?Type_key,?Element}],
              [{?Component_key,?VPC},
               {?Marker_key,5},
               {?Coords_key,[{?X0_key,1},{?X1_key,6},{?Y0_key,3},{?Y1_key,6}]},
               {?Type_key,?Group}]],

  Data_received = marker_translator:terraform(Raw_data),

  Resources = lists:filter(fun(X) -> orddict:is_key(?Terraform_resource_key, X) end, Data_received),

  ?assertEqual(3, length(Resources)).
