g1([person(kara, [barry, clark]),
    person(bruce, [clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

% ------------------------------------------------------------------------------

% convenience functions, mainly list operations
equal(X,X).

elem(E, [E|_]).
elem(E, [_|T]) :-
  elem(E, T).

select1(X, [X|T], T).
select1(X, [H|T], [H|R]) :-
  select1(X, T, R).

% checks whether X and Y are different members of the network
% by use of select instead of not
different(G, X, Y) :-
  elem(person(X, _), G),
  elem(person(Y, _), G),
  select1(person(X, _), G, R),
  elem(person(Y, _), R).

% checks if somebody is not member of a list of people (subset of G)
% by checking if he is different to every member
not_in(_, _, []).
not_in(G, X, [H|T]) :-
  different(G, X, H),
  not_in(G, X, T).

%-------------------------------------------------------------------------------


likes([H|_], X, Y) :-
  equal(H, person(X, L)),
  elem(Y, L).
likes([_|T], X, Y) :-
  likes(T, X, Y).


dislikes(G, X, Y) :-
  likes(G, Y, X),
  different(G, X, Y),
  elem(person(X, Fs), G),
  not_in(G, Y, Fs).

%---------------------------------------

popular(G, X) :-
  elem(person(X, Fs), G),
  all_like(G, Fs, X).

all_like(_, [], _).
all_like(G, [H|T], Y) :-
  likes(G, H, Y),
  all_like(G, T, Y).

%---------------------------------------

outcast(G, X) :-
  elem(person(X, Fs), G),
  all_dislike(G, Fs, X).

all_dislike(_, [], _).
all_dislike(G, [H|T], Y) :-
  dislikes(G, H, Y),
  all_dislike(G, T, Y).

%---------------------------------------

friendly(G, X) :-
  find_friends(G, G, X, [], Fs),
  likes_all(G, X, Fs).

% finds all the people in G who like X
find_friends(_, [], _, Acc, Acc).
find_friends(G, [H|T], X, Acc, Res) :-
  equal(H, person(Y, YFs)),
  elem(X, YFs),                        % friend found
  find_friends(G, T, X, [Y|Acc], Res). % append him
find_friends(G, [H|T], X, Acc, Res) :-
  equal(H, person(_, YFs)),
  not_in(G, X, YFs),                   % no friend
  find_friends(G, T, X, Acc, Res).     % no append

likes_all(_, _, []).
likes_all(G, X, [H|T]) :-
  likes(G, X, H),
  likes_all(G, X, T).

%---------------------------------------

hostile(G, X) :-
  find_friends(G, G, X, [], Fs),
  dislikes_all(G, X, Fs).

dislikes_all(_, _, []).
dislikes_all(G, X, [H|T]) :-
  dislikes(G, X, H),
  dislikes_all(G, X, T).

%---------------------------------------
