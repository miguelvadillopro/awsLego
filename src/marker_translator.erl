-module(marker_translator).
-export([markers_midpoint_coords/1]).

%type_filter(Group) ->
%    lists:filter(fun(X) -> length(orddict:filter(fun(Y,Z) -> Z == 170 end, X)) > 0 end, L).

markers_midpoint_coords(List) -> markers_midpoint_coords(List, []).
markers_midpoint_coords([], List) -> List;
markers_midpoint_coords([Head|Tail], List) -> markers_midpoint_coords(Tail, lists:append(marker_midpoints(Head),List)).

marker_midpoints(List) ->

    Marker = orddict:fetch("marker", orddict:from_list(List)),
    Coords = orddict:erase("marker", orddict:from_list(List)),

    {Points_x, Points_y} = get_coords(Coords),

    {X0, X1} = get_midpoints(Points_x),
    {Y0, Y1} = get_midpoints(Points_y),

    [[{"marker", Marker},{"coords",[{"X0", X0},{"X1", X1},{"Y0", Y0},{"Y1", Y1}]}]].


get_coords(Coords) -> get_coords(Coords, [], [], orddict:fetch_keys(Coords)).
get_coords(_, X, Y, []) -> {X, Y};
get_coords(Coords, X, Y, [Key|Keys]) ->


    [Px|[Py|_]] = orddict:fetch(Key, orddict:from_list(Coords)),
    get_coords(Coords,lists:append(X,[Px]), lists:append(Y,[Py]), Keys).

get_midpoints(Points) ->

    [P0, P1, P2, P3] = lists:sort(Points),
    {(P0 + P1) / 2, (P2 + P3) / 2}.

