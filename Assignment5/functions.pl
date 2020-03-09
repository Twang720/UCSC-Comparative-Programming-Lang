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
% Calculates distance between two airports
%
distance( A1, A2, Dist) :-
   airport( A1, _, degmin( Deg1, Min1), degmin( Deg2, Min2)),
   airport( A2, _, degmin( Deg3, Min3), degmin( Deg4, Min4)),
   deg_to_radians( Deg1, Min1, Lat1),
   deg_to_radians( Deg2, Min2, Long1),
   deg_to_radians( Deg3, Min3, Lat2),
   deg_to_radians( Deg4, Min4, Long2),
   haversine_radians( Lat1, Long1, Lat2, Long2, Dist).

%
% Converts distance to hours
%
dist_to_hours( Dist, Hours) :-
   Hours is Dist/500.

%
% Converts hours to mintues
%
hours_to_minutes( Hours, Mins) :-
   Mins is Hours * 60.

%
% Find a path from one node to another.
%
writeallpaths( Node, Node) :-
   write( Node), write( ' is '), write( Node), nl.
writeallpaths( A1, A2) :-
   airport( A1, Name1, _, _),
   airport( A2, Name2, _, _),
   listpath( A1, A2, [Node], List),
   write( Name1), write( ' to '), write( Name2), write( ' is '),
   writepath( List),
   fail.

writepath( []) :-
   nl.
writepath( [Head|Tail]) :-
   write( ' '), write( Head), writepath( Tail).

listpath( Node, End, Outlist) :-
   listpath( Node, End, [Node], Outlist).

listpath( Node, Node, _, [Node]).
listpath( Node, End, Tried, [Name|List]) :-
   flight( Node, Next, _),
   airport( Node, Name, _, _),
   not( member( Next, Tried)),
   listpath( Next, End, [Next|Tried], List).
