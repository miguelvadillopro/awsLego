-module(marker_translator).
-export([markers_midpoint_coords/1, markers_classification/1, grouping_elements/1, connecting_elements/1, terraform_translation/1,
         grouping_markers/1, to_terraform/1]).
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
markers_classification([], Components) -> Components;
markers_classification([Head|Tail], Components) -> markers_classification(Tail, lists:append([classify_marker(Head, [?Types, ?Components])],Components)).

classify_marker(Marker, List_classes) -> classify_marker(Marker, orddict:fetch(?Marker_key, orddict:from_list(Marker)), List_classes).
classify_marker(Marker, _, []) -> Marker;
classify_marker(Marker, Id, [Head|Tail]) -> {K, V} = get_classification(Id, Head),
                                            if
                                                K /= null andalso V /= null -> classify_marker(orddict:store(K, V, Marker), Id, Tail);
                                                true -> classify_marker(Marker, Id, Tail)
                                            end.


get_classification(Id, Classes) -> get_classification(Id, Classes, {}, false).
get_classification(_, _, Class, true) -> {orddict:fetch("Key", orddict:from_list(Class)), orddict:fetch("Value", orddict:from_list(Class))};
get_classification(_, [], _, _) -> {null, null};
get_classification(Id, [Head|Tail], _, _) -> Element_class = lists:member(Id,orddict:fetch("range", orddict:from_list(Head))),
                                             get_classification(Id, Tail, Head, Element_class).








grouping_elements(Components) ->
    Sorted_components = lists:sort(fun(A, B) -> orddict:fetch(?Sorting_key, orddict:from_list(A)) <
                                                orddict:fetch(?Sorting_key, orddict:from_list(B)) end, Components),
    grouping_elements(Sorted_components, []).

grouping_elements([], Components) -> Components;
grouping_elements([Component|Remaining_components], Components) ->

    Updated_element = element_with_groups(Components, Component, orddict:fetch(?Type_key, Component)),
    grouping_elements(Remaining_components, lists:append([Updated_element], Components)).


element_with_groups(_, Component, Type) when Type == ?Connection -> Component;
element_with_groups([], Element, _) -> Element;
element_with_groups([Head|Tail], Element, _) -> element_with_groups(Tail, element_in_group(Head, Element), []).


element_in_group(Group, Element) -> element_in_group(Group, Element, [{?X1_key,?X0_key},{?Y1_key,?Y0_key}], true).
element_in_group(_, Element, _, false) -> Element;
%element_in_group(Group, Element, [], true) -> orddict:append_list(?Groups_key, [orddict:fetch(?Marker_key, Group)], Element);
element_in_group(Group, Element, [], true) -> orddict:append_list(?Groups_key, Group, Element);
element_in_group(Group, Element, [{K1,K0}|Tail],_) ->

    Element_coords = orddict:fetch(?Coords_key, orddict:from_list(Element)),
    Group_coords = orddict:fetch(?Coords_key, orddict:from_list(Group)),
    element_in_group(Group, Element, Tail,
                     (orddict:fetch(K1, orddict:from_list(Group_coords)) > orddict:fetch(K1, orddict:from_list(Element_coords))) andalso
                     (orddict:fetch(K0, orddict:from_list(Group_coords)) < orddict:fetch(K0, orddict:from_list(Element_coords)))).







connecting_elements(Components) ->

  Connections = lists:filter(fun(X) -> orddict:fetch(?Type_key, orddict:from_list(X)) == ?Connection end, Components),

  Components_with_connections = get_connections(Components, Connections),

  connection_ponderation(Components_with_connections).


get_connections (Components, Connections) -> get_connections(Components, Connections, []).
get_connections ([], _, Components) -> Components;
get_connections ([Head | Tail], Connections, Components) ->
  Type_head = orddict:fetch(?Type_key, orddict:from_list(Head)),

  if

    Type_head == ?Element ->
      get_connections (Tail, Connections, lists:append([component_connections(Head, Connections)],Components));

    true ->
      get_connections (Tail, Connections, lists:append([Head],Components))

  end.


component_connections (Element, Connections) ->

  Atoms = [[x, final], [x, initial], [y, final], [y, initial]],

  component_connections(Atoms, [], Element, Connections).

component_connections ([], Element_connections, Element, _) -> orddict:append_list(?Connections_key, Element_connections, Element);

component_connections ([[x, Border]|Tail], Element_connections, Element, Connections) ->

  Element_coords = orddict:fetch(?Coords_key, orddict:from_list(Element)),

  X_connections = lists:filter(fun(Connection) ->
                                    Connection_coords = orddict:fetch(?Coords_key, orddict:from_list(Connection)),
                                    connection_filter(Element_coords, Connection_coords, ?Delta_x, ?X0_key, ?X1_key, Border) andalso
                                    inside_element_range(Connection_coords, Element_coords, ?Y0_key, ?Y1_key) end, Connections),

  Element_connections_update = lists:append([X_connections], Element_connections),

  component_connections(Tail, Element_connections_update, Element, Connections);


component_connections ([[y, Border]|Tail], Element_connections, Element, Connections) ->

  Element_coords = orddict:fetch(?Coords_key, orddict:from_list(Element)),

  Y_connections = lists:filter(fun(Connection) ->
                                    Connection_coords = orddict:fetch(?Coords_key, orddict:from_list(Connection)),
                                    connection_filter(Element_coords, Connection_coords, ?Delta_y, ?Y0_key, ?Y1_key, Border) andalso
                                    inside_element_range(Connection_coords, Element_coords, ?X0_key, ?X1_key) end, Connections),

  Element_connections_update = lists:append([Y_connections], Element_connections),

  component_connections(Tail, Element_connections_update, Element, Connections).



connection_filter (Connection, Element, Delta, A0, A1, initial) -> connection_filter (Element, Connection, Delta, A0, A1, final);

connection_filter (Element, Connection, Delta, A0, A1, final) ->
  (orddict:fetch(A1, orddict:from_list(Element)) + Delta >=
   orddict:fetch(A0, orddict:from_list(Connection)) - Delta andalso
   orddict:fetch(A1, orddict:from_list(Element)) + Delta =<
   orddict:fetch(A0, orddict:from_list(Connection)) + Delta).

inside_element_range(Connection, Element, B0, B1) ->
  orddict:fetch(B0, orddict:from_list(Element)) =<
  orddict:fetch(B0, orddict:from_list(Connection)) andalso
  orddict:fetch(B1, orddict:from_list(Element)) >=
  orddict:fetch(B1, orddict:from_list(Connection)).


connection_ponderation(Components) ->

  Elements = lists:filter(fun(X) -> orddict:fetch(?Type_key, orddict:from_list(X)) == ?Element end, Components),
  Components_wo_elements = Components -- Elements,
  connection_ponderation(Elements, Components_wo_elements, Elements).

connection_ponderation([], Components, _) -> Components;
connection_ponderation([Head|Tail], Components, Elements) -> 


  Connected_elements = lists:filter(fun(Element) -> length(orddict:fetch(?Connections_key, orddict:from_list(Head))) >
                                                    length(orddict:fetch(?Connections_key, orddict:from_list(Head)) --
                                                           orddict:fetch(?Connections_key, orddict:from_list(Element))) end, Elements),

  Type = orddict:fetch(?Component_key, orddict:from_list(Head)),


  Filtered_elements = lists:filter(fun(Element) -> length(orddict:fetch(Type, orddict:from_list(?Connections_ponderation))) >
                                                   length(orddict:fetch(Type, orddict:from_list(?Connections_ponderation)) --
                                                          [orddict:fetch(?Component_key, orddict:from_list(Element))]) end, Connected_elements),


  if length(Filtered_elements) > 0 ->
       Head_with_groups = orddict:append_list(?Groups_key, [Filtered_elements], Head),
       Head_wo_connections = orddict:erase(?Connections_key, Head_with_groups),

       connection_ponderation(Tail, lists:append([Head_wo_connections],Components),Elements);

     true ->

       Head_wo_connections = orddict:erase(?Connections_key, Head),

       connection_ponderation(Tail, lists:append([Head_wo_connections],Components),Elements)

  end.



terraform_translation(Components) ->
  Components_without_Connections = lists:filter(fun(X) -> orddict:fetch(?Type_key, orddict:from_list(X)) /= ?Connection end, Components),
  terraform_translation(Components_without_Connections, [], component).
terraform_translation(Components, dependency) -> terraform_translation(Components, [], dependency).
terraform_translation([], Components, _) -> Components;
terraform_translation([Head|Tail], Components, Atom) -> terraform_translation(Tail, lists:append([terraform_template(Head, Atom)], Components), Atom).

terraform_template(Component, component) -> 

  Marker = orddict:fetch(?Marker_key, orddict:from_list(Component)),

  Template_resource = orddict:fetch(Marker, orddict:from_list(?Terraform_template)),

  Have_elements = orddict:is_key(?Groups_key, orddict:from_list(Component)),


  if Have_elements ->

       Dependencies = terraform_translation(orddict:fetch(?Groups_key, orddict:from_list(Component)), dependency),
       Template_dependencies = [{?Terraform_dependency_key, Dependencies}],
       Template_integrated = orddict:merge(fun(_, V1, _) -> V1 end, Template_resource, Template_dependencies),


       Resource = [{Marker, Template_integrated}],
       [{?Terraform_resource_key, Resource}];

     true -> Resource = [{Marker, Template_resource}],
             [{?Terraform_resource_key, Resource}]
  end;




terraform_template(Component, dependency) ->

  erlang:display("Component Debug"),
  erlang:display(Component),
  Marker = orddict:fetch(?Marker_key, orddict:from_list(Component)),
  Component_name = orddict:fetch(?Component_key, orddict:from_list(Component)),

  Component_name ++ "." ++ integer_to_list(Marker).







grouping_markers(Components) ->

  Connections = get_connections(Components),
  Components_wo_connections = Components -- Connections,
  Markers = get_markers_set(Components_wo_connections),
  grouping_markers(Markers, Connections, Components_wo_connections).
grouping_markers([], Grouped_components, _) -> Grouped_components;
grouping_markers([Marker|Remaining_markers], Grouped_components, Components) ->
  Coords = get_max_coords(Marker, Components),
  grouping_markers(Remaining_markers, lists:append([[{?Marker_key, Marker},{?Coords_key, Coords}]], Grouped_components), Components).


get_connections(Components) ->

  Markers_range = lists:filter(fun(X)-> orddict:fetch("Value",orddict:from_list(X)) == ?Connection end, ?Types),
  [Connections_markers|_] = Markers_range,
  lists:filter(fun(X) ->
                   lists:member(
                     orddict:fetch(?Marker_key, orddict:from_list(X)),
                     orddict:fetch("range", orddict:from_list(Connections_markers))) end, Components).


get_markers_set(Components) -> get_markers_set(Components, []).
get_markers_set([], Markers) -> Set_of_markers = sets:from_list(Markers),
                                sets:to_list(Set_of_markers);
get_markers_set([Component|Remaining_components], Markers) ->
  get_markers_set(Remaining_components, lists:append([orddict:fetch(?Marker_key, orddict:from_list(Component))], Markers)).

get_max_coords(Marker, Components) ->

  Components_same_marker = lists:filter(fun(X) -> orddict:fetch(?Marker_key, orddict:from_list(X)) == Marker end, Components),

  max_coords(Components_same_marker, []).

max_coords([], Coords) ->

  X_coords = orddict:fetch(x, Coords),
  Y_coords = orddict:fetch(y, Coords),

  X_sorted_coords = lists:sort(X_coords),
  Y_sorted_coords = lists:sort(Y_coords),

  [X0|_] = X_sorted_coords,
  [X1|_] = lists:reverse(X_sorted_coords),
  [Y0|_] = Y_sorted_coords,
  [Y1|_] = lists:reverse(Y_sorted_coords),

  [{?X0_key, X0},{?X1_key, X1},{?Y0_key, Y0},{?Y1_key, Y1}];


max_coords([Component|Remaining_components], Coords) ->
  Coords_component = orddict:fetch(?Coords_key,orddict:from_list(Component)),

  X0 = orddict:fetch(?X0_key, Coords_component),
  X1 = orddict:fetch(?X1_key, Coords_component),
  Y0 = orddict:fetch(?Y0_key, Coords_component),
  Y1 = orddict:fetch(?Y1_key, Coords_component),

  Xs = lists:append([X0],[X1]),
  Ys = lists:append([Y0],[Y1]),

  Coords_x = orddict:append_list(x, Xs, Coords),
  Coords_y = orddict:append_list(y, Ys, Coords_x),


  max_coords(Remaining_components, Coords_y).



to_terraform(Markers) ->

  terraform_translation(connecting_elements(grouping_elements(markers_classification(grouping_markers(markers_midpoint_coords(Markers)))))).

