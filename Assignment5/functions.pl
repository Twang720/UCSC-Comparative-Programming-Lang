% functions.pl
% Timothy Wang tqwang
% Eric Mar emmar

%
% Prolog version of not.
%
not( X) :- X, !, fail.
not( _).

%
% Returns list of math functions on X
%
mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

%
% Returns list of constants
%
constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

%
% Some function that I don't use
%
sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

%
% Computes distance between two coordinates
%
haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.

%
% Converts degrees and minutes to radians
%
deg_to_radians( Deg, Min, Radian) :-
   Radian is (Deg + Min / 60) * pi / 180.

%
% Prints the airport information given code
%
show( Code) :-
   airport( Code, City, Nlat, Wlong),
   write( Code), write( ' '), write( City),
   write( ' Nlat='), write( Nlat),
   write( ' Wlong='), write( Wlong), nl.

%
% Calculates time in hours between two airports
%
hours( A1, A2, Time) :-
   airport( A1, _, degmin( Deg1, Min1), degmin( Deg2, Min2)),
   airport( A2, _, degmin( Deg3, Min3), degmin( Deg4, Min4)),
   deg_to_radians( Deg1, Min1, Lat1),
   deg_to_radians( Deg2, Min2, Long1),
   deg_to_radians( Deg3, Min3, Lat2),
   deg_to_radians( Deg4, Min4, Long2),
   haversine_radians( Lat1, Long1, Lat2, Long2, Distance),
   Time is Distance/500.

%
% Converts to singular time unit
%
minutes( Hours, Mins, Time) :-
   Time is (Hours * 60) + Mins.

%
% Converts hours to mintues
%
hours_and_minutes( Time, Hours, Mins) :-
   Hours is floor(Time),
   Temp is (Time - Hours) * 60,
   Mins is round(Temp).

%
% Formatting stuff to print
%
pad2( Num, Padded) :- Num < 10, string_concat( '0', Num, Padded).
pad2( Num, Num).

print_flight( Activity, Code, Name, Hour, Min) :-
   write( Activity), write( '  '),
   string_upper( Code, Upper),
   write( Upper), write( ' '), write( Name), write( ' '),
   pad2( Hour, Hour2), write( Hour2), write( ':'),
   pad2( Min, Min2), write( Min2), nl.

%
% Find a path from one node to another.
%
fly( Depart, Depart) :-
   write('Error: Can\'t fly to same airport.'), nl.
fly( Depart, Arrive) :-
   listpath( Depart, Arrive, List),
   writepath( List), !.

%
% Helper function that prints flights
%
writepath( []) :-
   nl.
writepath( [flight(Head, Head2, time(Hour, Min)) |Tail]) :-
   airport(Head, Name, _, _),
   airport(Head2, Name2, _, _),
   hours(Head, Head2, Time),
   hours_and_minutes(Time, Hour2, Min2),
   Temp is floor((Min + Min2)/60),
   Hour3 is Hour + Hour2 + Temp,
   Min3 is mod(Min + Min2,60),
   print_flight('depart', Head, Name, Hour, Min),
   print_flight('arrive', Head2, Name2, Hour3, Min3),
   writepath( Tail).

listpath( Node, End, Outlist) :-
   listpath( Node, End, [Node], 0, Outlist).

listpath( Node, Node, _, _, []).
listpath( Node, End, Tried, Time, [flight(Node, Next, time(Hour, Minute))|List]) :-
   flight( Node, Next, time(Hour, Minute)),
   not( member( Next, Tried)),
   minutes(Hour, Minute, T1),
   not( Time > T1),
   hours( Node, Next, T),
   hours_and_minutes(T, Hour2, Minute2),
   minutes(Hour2, Minute2, T2),
   Current is T1 + T2 + 30,
   listpath( Next, End, [Next|Tried], Current, List).
