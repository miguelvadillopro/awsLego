-module(marker_translator).
-export([markers_midpoint_coords/1, markers_classification/1]).
-include("constants.hrl").

%type_filter(Group) ->
%    lists:filter(fun(X) -> length(orddict:filter(fun(Y,Z) -> Z == 170 end, X)) > 0 end, L).

markers_midpoint_coords(List) -> markers_midpoint_coords(List, []).
markers_midpoint_coords([], List) -> List;
markers_midpoint_coords([Head|Tail], List) -> markers_midpoint_coords(Tail, lists:append(marker_midpoints(Head),List)).

marker_midpoints(List) ->

    Marker = orddict:fetch(?Marker_key, orddict:from_list(List)),
    Coords = orddict:erase(?Marker_key, orddict:from_list(List)),

    {Points_x, Points_y} = get_coords(Coords),

    {X0, X1} = get_midpoints(Points_x),
    {Y0, Y1} = get_midpoints(Points_y),

    [[{?Marker_key, Marker},{?Coords_key,[{?X0_key, X0},{?X1_key, X1},{?Y0_key, Y0},{?Y1_key, Y1}]}]].


get_coords(Coords) -> get_coords(Coords, [], [], orddict:fetch_keys(Coords)).
get_coords(_, X, Y, []) -> {X, Y};
get_coords(Coords, X, Y, [Key|Keys]) ->


    [Px|[Py|_]] = orddict:fetch(Key, orddict:from_list(Coords)),
    get_coords(Coords,lists:append(X,[Px]), lists:append(Y,[Py]), Keys).

get_midpoints(Points) ->

    [P0, P1, P2, P3] = lists:sort(Points),
    {(P0 + P1) / 2, (P2 + P3) / 2}.








markers_classification(Markers) -> markers_classification(Markers, []).
markers_classification([], Markers) -> Markers;
markers_classification([Head|Tail], Markers) -> markers_classification(Tail, lists:append([classify_marker(Head, [?Types, ?Components])],Markers)).

classify_marker(Marker, List_classes) -> classify_marker(Marker, orddict:fetch(?Marker_key, orddict:from_list(Marker)), List_classes).
classify_marker(Marker, _, []) -> Marker;
classify_marker(Marker, Id, [Head|Tail]) -> {K, V} = get_classification(Id, Head),
                                            classify_marker(orddict:store(K, V, Marker), Id, Tail).

get_classification(Id, Classes) -> get_classification(Id, Classes, {}, false).
get_classification(_, _, Class, true) -> {orddict:fetch("Key", orddict:from_list(Class)), orddict:fetch("Value", orddict:from_list(Class))};
get_classification(Id, [Head|Tail], _, _) -> Element_class = lists:member(Id,orddict:fetch("range", orddict:from_list(Head))),
                                             get_classification(Id, Tail, Head, Element_class).

