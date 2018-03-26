
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
-define(Component_key, "component").
-define(Groups_key, "groups").
-define(Connections_key, "connections").
-define(Delta_x, 0.5).
-define(Delta_y, 0.5).

-define(Terraform_dependency_key, "depends_on").
-define(Terraform_resource_key, "resource").

-define(EC2,"aws_instance").
-define(S3, "aws_s3_bucket").
-define(VPC, "aws_vpc").
-define(Sub_net, "sub_net").

-define(Sorting_key, ?Coords_key).

-define(Types, [[{"range",lists:seq(1,10)},{"Key", ?Type_key},{"Value", ?Group}],
                [{"range",lists:seq(11,20)},{"Key", ?Type_key},{"Value", ?Element}],
                [{"range",lists:seq(21,30)},{"Key", ?Type_key},{"Value", ?Connection}]]).


-define(Components, [[{"range",lists:seq(1,5)},{"Key", ?Component_key},{"Value", ?VPC}],
                     [{"range",lists:seq(6,10)},{"Key", ?Component_key},{"Value", ?Sub_net}],
                     [{"range",lists:seq(11,15)},{"Key", ?Component_key},{"Value", ?EC2}],
                     [{"range",lists:seq(16,20)},{"Key", ?Component_key},{"Value", ?S3}]]).

-define(Connections_ponderation, [{?EC2,[?S3]}, {?S3,[]}]).

-define(Terraform_template, [{12, [{"ami", "ami-2757f631"},{"instance_type", "t2.micro"}]},
                             {17, [{"bucket", "terraform-getting-starteg-guide"},{"acl", "private"}]},
                             {5, [{"cidr_block", "10.0.0.0/16"}]}]).
