
-define(Marker_key,"marker").
-define(Coords_key,"coords").
-define(X0_key, "X0").
-define(X1_key, "X1").
-define(Y0_key, "Y0").
-define(Y1_key, "Y1").
-define(Type_key, "type").
-define(Group, "Group").
-define(Element, "Element").
-define(Connection, "Connection").
-define(Min_group_marker, 1).
-define(Max_group_marker, 10).
-define(Min_element_marker, 11).
-define(Max_element_marker, 20).
-define(Min_connection_marker, 21).
-define(Max_connection_marker, 30).
-define(Component_key, "Component").

-define(EC2, ec2).
-define(S3, s3).
-define(VP, vp).
-define(Sub_net, sub_net).

-define(Types, [[{"range",lists:seq(1,10)},{"Key", ?Type_key},{"Value", ?Group}],
                [{"range",lists:seq(11,20)},{"Key", ?Type_key},{"Value", ?Element}],
                [{"range",lists:seq(21,30)},{"Key", ?Type_key},{"Value", ?Connection}]]).


-define(Components, [[{"range",lists:seq(1,5)},{"Key", ?Component_key},{"Value", ?VP}],
                     [{"range",lists:seq(6,10)},{"Key", ?Component_key},{"Value", ?Sub_net}],
                     [{"range",lists:seq(11,15)},{"Key", ?Component_key},{"Value", ?EC2}],
                     [{"range",lists:seq(16,20)},{"Key", ?Component_key},{"Value", ?S3}]]).

