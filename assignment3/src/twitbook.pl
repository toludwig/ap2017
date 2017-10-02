g1([person(kara, [barry, clark]),
    person(bruce, [clark, oliver]),
    person(barry, [kara, oliver]),
    person(clark, [oliver, kara]),
    person(oliver, [kara])]).

g2([person(batman, [green_arrow, superman]),
    person(green_arrow, [supergirl]),
    person(supergirl, [flash, superman]),
    person(flash, [green_arrow, supergirl]),
    person(superman, [green_arrow, supergirl])]).

% ------------------------------------------------------------------------------
% easy convenience functions

equal(X,X).

elem(E, [E|_]).
elem(E, [_|T]) :-
  elem(E, T).

member(G, X, XFs) :-
  elem(person(X, XFs), G).

% alias for member
friends(G, X, XFs) :-
  elem(person(X, XFs), G).

names([], []).
names([H|T], [X|R]) :-
  equal(H, person(X, _)),
  names(T, R).

%---------------------------------------
% the real fancy stuff, that prevents NOT...

% extracts the element X of a list and returns the rest,
% fails if X is not in the list
select1(X, [X|T], T).
select1(X, [H|T], [H|R]) :-
  select1(X, T, R).

% checks whether X and Y are different members of the network
% by use of select; note that we need G for this!
different(G, X, Y) :-
  %member(G, X, _),
  %member(G, Y, _),
  select1(person(X, _), G, R),
  elem(person(Y, _), R).

% checks if somebody is not member of a list of people (subset of G)
% by checking if he is different from every member
not_in(_, _, []).
not_in(G, X, [H|T]) :-
  different(G, X, H),
  not_in(G, X, T).

% filters a list L1 (of names) for people who are also in list L2
filter(G, L1, L2, Intersect) :-
  filter1(G, L1, L2, [], Intersect).

filter1(_, [], _, Acc, Acc).
filter1(_, [H|T], L2, Acc, Res) :-
  elem(H, L2),
  filter1(H, T, L2, [H|Acc], Res).
filter1(G, [H|T], L2, Acc, Res) :-
  not_in(G, H, L2),
  filter1(G, T, L2, Acc, Res).

%-------------------------------------------------------------------------------
% local relations

likes([H|_], X, Y) :-
  equal(H, person(X, L)),
  elem(Y, L).
likes([_|T], X, Y) :-
  likes(T, X, Y).

dislikes(G, X, Y) :-
  likes(G, Y, X),
  different(G, X, Y),
  member(G, X, XFs),
  not_in(G, Y, XFs).

% helper: a weaker version of dislikes, X and Y may have no relation at all
not_likes(G, X, Y) :-
  different(G, X, Y),
  member(G, X, XFs),
  not_in(G, Y, XFs).

%---------------------------------------

popular(G, X) :-
  member(G, X, XFs),
  all_like(G, XFs, X).

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

%-------------------------------------------------------------------------------
% global relations

% admires is prone to cycles, we need to keep track of people we already visited
admires(G, X, Y) :-
  different(G, X, Y),
  admires1(G, G, X, Y).

admires1(G, _, X, Y) :-
  likes(G, X, Y).
admires1(G, GTodo, X, Y) :-
  member(GTodo, Z, _),
  likes(G, X, Z),
  select1(person(Z, _), GTodo, GTodo1),
  admires1(G, GTodo1, Z, Y).

%--------------------------------------

% indifferent needs again a cycle check
indifferent(G, X, Y) :-
  names(G, Todo),
  indifferent(G, Todo, X, Y).

indifferent(G, Todo, X, Y) :-
  not_likes(G, X, Y),              % X does not like Y himself
  member(G, X, XFs),               % and X's friends are also
  select1(X, Todo, Todo1),
  all_indifferent(G, Todo1, XFs, Y). % all indifferent to Y

all_indifferent(_, _, [], _).
all_indifferent(G, Todo, [H|T], Y) :-
  member(Todo, H, _),             % check if H is still in Todos
  indifferent(G, GTodo, H, Y),
  all_indifferent(G, GTodo, T, Y).

%-------------------------------------------------------------------------------
% whole-graph properties

same_world(G1, G2, Map) :-
  same_length(G1, G2), % quick sanity check first
  names(G1, N1),       % get the names for convenience
  names(G2, N2),
  % stupid generate and test approach follows ...
  generate_mapping(N1, N2, Map),
  test_mapping(G1, G2, Map).

generate_mapping([], _, []).
generate_mapping([H|T], N2, [(H,N)|Map]) :-
  select1(N, N2, N2R),
  generate_mapping(T, N2R, Map).

test_mapping([], _, _).
test_mapping([H|T], G2, Map) :-
  equal(H, person(N1, F1)),
  elem((N1,N2), Map),   % lookup the corresponding N2
  friends(G2, N2, F2),  % and his friends;
  same_length(F1, F2),  % necessary, not only sanity check because
  same_friends(F1, F2, Map), % this checks only if F1 subset_of F2
  test_mapping(T, G2, Map).

same_friends([], _, _).
same_friends([N1|T], F2, Map) :-
  elem((N1,N2), Map),              % lookup in mapping
  elem(N2, F2),                    % check if N2 is in F2
  same_friends(T, F2, Map).

same_length([], []).
same_length([_|T1], [_|T2]) :-
  same_length(T1, T2).
