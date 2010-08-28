% A client can move up and down
-define(UP, 1).
-define(DOWN, 2).

% Positions of the field
-define(FX0, 22).
-define(FY0, 4).

% Dimensions of the field
-define(FX, 40).
-define(FY, 20).

% Dimensions of the bars of the clients
-define(CY, 3).
-define(CX, 1).

% Dimensions of the ball
-define(BX, 2).
-define(BY, 1).

% Dimensions of the numbers of the score
-define(NX, 6).
-define(NY, 5).

% Positions first number of result
-define(N1X0, 9).
-define(N1Y0, 11).

% Positions second number of result
-define(N2X0, ?FX0 + ?FX + 7).
-define(N2Y0, 11).

% Round length
-define(ROUND_LENGTH, 100).

% Pause after goal
-define(PAUSE_AFTER_GOAL, 3000).

% Color pairs
-define(CLIENT_PAIR, 1).
-define(BALL_PAIR, 2).
-define(FIELD_PAIR, 3).
-define(RESULT_PAIR, 4).
-define(BG_PAIR, 5).

-record(ball, {x = (?FX + ?FX0) div 2,
	       y = (?FY + ?FY0) div 2,
	       speed = 1,
	       degrees = 135,
	       path = [{(?FX + ?FX0) div 2, (?FY + ?FY0) div 2}]}).

-record(client, {id,
		 x = 0,
		 y = 0,
		 path = []}).
