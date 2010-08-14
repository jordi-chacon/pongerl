% A client can move up and down
-define(UP, 1).
-define(DOWN, 2).

% Dimensions of the field
-define(X0, 0).
-define(Y0, 0).
-define(XF, 80).
-define(YF, 20).

-record(ball, {x = (?XF - ?X0) div 2,
	       y = (?YF - ?Y0) div 2,
	       speed = 1,
	       degrees = 180}).
