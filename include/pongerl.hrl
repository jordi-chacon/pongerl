% A client can move up and down
-define(UP, 1).
-define(DOWN, 2).

% Dimensions of the field
-define(X0, 0).
-define(Y0, 0).
-define(XF, 40).
-define(YF, 20).

% Dimensions of the bars of the clients
-define(CY, 3).
-define(CX, 1).

% Dimensions of the ball
-define(BX, 2).
-define(BY, 1).

% Round length
-define(ROUND_LENGTH, 100).

% Color pairs
-define(CLIENT_PAIR, 1).
-define(BALL_PAIR, 2).
-define(FIELD_PAIR, 3).

-record(ball, {x = (?XF - ?X0) div 2,
	       y = (?YF - ?Y0) div 2,
	       speed = 1,
	       degrees = 180,
	       path = [{(?XF - ?X0) div 2, (?YF - ?Y0) div 2}]}).

-record(client, {id,
		 x = 0,
		 y = 0,
		 path = []}).
