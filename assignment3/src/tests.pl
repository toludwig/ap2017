:-include(twitbook).

warmup_test(G) :-
  warm_test(G),
  \+(warm_err(G)),
  write("Warmup predicates fail on incorrect inputs").

warm_test(G) :-
  likes(G,kara,barry),
  likes(G,kara,clark),
  dislikes(G,kara,oliver),
  write("Warmup predicates pass on correct inputs").


warm_err(G) :-
  dislikes(G,kara,clark);
  likes(G,oliver,bruce).

local_test(G1,G2) :-
  pop_test(G1,G2),
  \+(pop_err(G1,G2)),
  out_test(G1,G2),
  \+(out_err(G1,G2)),
  friendly_test(G1,G2),
  \+(friendly_err(G1,G2)),
  host_test(G1,G2),
  \+(host_err(G1,G2)).

pop_test(G1,G2) :-
  popular(G1, kara),
  popular(G2, supergirl).

pop_err(G1,G2) :-
  popular(G1, bruce);
  popular(G1, clark);
  popular(G2, green_arrow).

out_test(G1,G2) :-
  outcast(G1, bruce),
  outcast(G1, oliver),
  outcast(G2, batman).

out_err(G1,G2) :-
  outcast(G1, kara);
  outcast(G1, clark);
  outcast(G2, flash).

friendly_test(G1,G2) :-
  friendly(G1, barry),
  friendly(G2, flash).

friendly_err(G1,G2) :-
  friendly(G1, clark);
  friendly(G2, supergirl).

host_test(G1,G2) :-
  hostile(G1, oliver),
  hostile(G1, bruce),
  hostile(G2, green_arrow).

host_err(G1,G2) :-
  hostile(G1, kara);
  hostile(G1, barry);
  hostile(G2, superman).

global_test(G1,G2) :-
  adm_test(G1,G2),
  \+(adm_err(G1,G2)),
  indif_test(G1,G2),
  \+(indif_err(G1,G2)).

adm_test(G1,G2) :-
  admires(G1, bruce, kara),
  admires(G1, clark, barry),
  admires(G1, oliver, clark),
  admires(G2, supergirl, green_arrow).

adm_err(G1,G2) :-
  admires(G1, kara, bruce);
  admires(G1, oliver, bruce);
  admires(G2, superman, batman).

indif_test(G1,G2) :-
  indifferent(G1, kara, bruce),
  indifferent(G1, oliver, bruce),
  indifferent(G2, superman, batman).

indif_err(G1,G2) :-
  indifferent(G1, bruce, kara);
  indifferent(G1, clarke, barry);
  indifferent(G1, oliver, clark);
  indifferent(G2, supergirl, green_arrow).
