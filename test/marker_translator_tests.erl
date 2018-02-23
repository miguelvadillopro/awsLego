-module(marker_translator_tests).
-include_lib("eunit/include/eunit.hrl").

%group_test() ->
%    Group = [{"marker", 170}, {0, [2,10]}, {1, [10,10]},{2,[2,2]},{3,[10,2]}],
%%    ?assertEqual(0, markerTranslator:togroup(Group)).

happy_path_markers_midpoint_calculation_test() ->
    Raw_data = [[{"marker",170},{0,[8,2]},{1,[9,8]},{2,[3,9]},{3,[2,4]}],[{"marker",123},{0,[6,1]},{1,[10,9]},{2,[4,10]},{3,[1,7]}]],
    Data_received = marker_translator:markers_midpoint_coords(Raw_data),
    ?assertNotEqual([],Data_received),
    ?assertEqual(length(Raw_data),length(Data_received)),
    ?assert(all_keys(["X0","Y0", "X1", "Y1"], Data_received )).

empty_list_markers_midpoint_calculation_test() ->
    Data_received = marker_translator:markers_midpoint_coords([]),
    ?assertEqual([], Data_received).

coords_rotated_same_midpoints_test() ->
    Data = [[{"marker", 23},{0,[3,2]},{1,[2,3]},{2,[0,2]},{3,[1,1]}]],
    Rotated_data = [[{"marker", 23},{0,[1,1]},{1,[3,2]},{2,[2,3]},{3,[0,2]}]],
    Coords = marker_translator:markers_midpoint_coords(Data),
    Rotated_coords = marker_translator:markers_midpoint_coords(Rotated_data),
    ?assertEqual(lists:sort(Coords), lists:sort(Rotated_coords)).

all_keys(Keys, Target_list) -> all_keys(Keys, Target_list, true).
all_keys(_, [], true) -> true;
all_keys(_, _, false) -> false;
all_keys(Keys, [Head|Tail], _) ->
    Coords = orddict:fetch("coords", orddict:from_list(Head)),
    all_keys(Keys, Tail, lists:sort(orddict:fetch_keys(Coords)) =:= lists:sort(Keys)).







marker_to_group_assignation_test() ->
    Raw_data = [[{"marker",123},
                 {"coords",[{"X0",2.5},{"X1",8.0},{"Y0",4.0},{"Y1",9.5}]}],
                [{"marker",170},
                 {"coords",[{"X0",2.5},{"X1",8.5},{"Y0",3.0},{"Y1",8.5}]}]],

    Data_received = marker_translator:marker_classification(Raw_data),

    ?assert(lists:all(fun(X) -> (orddict:is_key("type", X)) end,Data_received)).

