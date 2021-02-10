%%%% -*- Mode: Prolog -*-

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic visited/2.
:- dynamic previous/3.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic row/3.
:- dynamic mst_arc/4.

% new_graph/1

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

% delete_graph/1

delete_graph(G) :-
   retractall(arc(G, _, _, _)),
   retractall(vertex(G, _)),
   retract(graph(G)).

% new_vertex/2

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- assert(vertex(G, V)), !.

% graph_vertices/2

graph_vertices(G, Vs) :- findall(V, vertex(G, V), Vs).

% list_vertices/1

list_vertices(G) :-
   graph(G),
   listing(vertex(G, _)).

% new_arc/4

new_arc(G, U, V, Weight) :- arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :-
   assert(arc(G, U, V, Weight)),
   assert(arc(G, V, U, Weight)), !.
new_arc(G, U, V) :- new_arc(G, U, V, 1).

% graph_arcs/2

graph_arcs(G, Es) :- findall(arc(G, U, V, W), arc(G, U, V, W), Es).

% vertex_neighbors/3

vertex_neighbors(G, V, Ns) :-
   vertex(G, V),
   findall(arc(G, V, N, W), arc(G, V, N, W), Ns).

% adjs/3

adjs(G, V, Vs) :-
   vertex(G, V),
   findall(vertex(G, N), arc(G, V, N, _), Vs).

% list_arcs/1

list_arcs(G) :-
   graph(G),
   listing(arc(G, _, _, _)).

% list_graph/1

list_graph(G) :-
   graph(G),
   list_vertices(G),
   list_arcs(G), !.

% read_graph/2

read_graph(G, FileName) :-
   new_graph(G),
   csv_read_file(FileName, Rows, [separator(0'\t)]),
   save_graph(G, Rows).

% save_graph/2

save_graph(_, []) :- !.
save_graph(G, [B | Bs]) :-
   functor(B, row, 3),
   arg(1, B, V),
   arg(2, B, U),
   arg(3, B, W),
   new_vertex(G, V),
   new_vertex(G, U),
   new_arc(G, V, U, W),
   save_graph(G, Bs), !.


% write_graph/2

write_graph(G, FileName) :- write_graph(G, FileName, graph).

% write_graph/3

write_graph(G, FileName, Type):-
   Type = graph,
   findall(row(U, V, W), arc(G, U, V, W), Rows),
   csv_write_file(FileName, Rows, [separator(0'\t)]), !.
write_graph(G, FileName, Type):-
   Type = edges,
   create_rows(G),
   findall(row(U, V, W), row(U, V, W), Rows),
   csv_write_file(FileName, Rows, [separator(0'\t)]),
   retractall(row(_, _, _)).

% create_rows/1

create_rows([]).
create_rows([A | As]) :-
   arg(2, A, U),
   arg(3, A, V),
   arg(4, A, W),
   Term =.. [row, U, V, W],
   assert(Term),
   create_rows(As).

% MST

% set_inf/2

set_inf(_, []).
set_inf(G, [V | Vs]):-
   assert(vertex_key(G, V, inf)),
   set_inf(G, Vs).

%mst_prim/2

mst_prim(G, Source) :-
   reset(),
   graph_vertices(G, Vs),
   set_inf(G, Vs),
   retract(vertex_key(G, Source, inf)),
   assert(vertex_key(G, Source, 0)),
   new_heap(myHeap),
   heap(H, 0),
   vertex_neighbors(G, Source, Ns),
   heap_insert_from_list(G, H, Ns),
   recursive_mst_prim(G), !.

recursive_mst_prim(_G) :-
   heap(_H, 0), !.
recursive_mst_prim(G) :-
   heap(H, _S),
   heap_extract(H, K, V), %recupero B
   retract(vertex_key(G, V, inf)),
   assert(vertex_key(G, V, K)),
   findall(arc(G, V, N, K), arc(G, V, N, K), Lvs),
   find_min_arc(G, Lvs, V),
   vertex_neighbors(G, V, Ns),
   heap_insert_from_list(G, H, Ns),
   recursive_mst_prim(G), !.

find_min_arc(G, [Lv | _Lvs], V):-
   arg(3, Lv, Neighbor),
   vertex_key(G, Neighbor, _),
   assert(vertex_previous(G, Neighbor, V)), !.
find_min_arc(G, [Lv | Lvs], V):-
   arg(3, Lv, Neighbor),
   not(vertex_key(G, Neighbor, _)),
   find_min_arc(G, Lvs, V).

% mst_get/3

mst_get(G, Source, PreorderTree) :-
   graph(G),
   findall(V, vertex_previous(G, Source, V), Vertexes), %trovo figli di source
   visit_mst(G, Source, Vertexes), %visito i figli
   findall(arc(G, V, N, W), mst_arc(G, V, N, W), Arcs),
   append(Arcs, [], PreorderTree).

% visit/4 - caso con lista ricorsivamente creata
/*
visit_mst(_, _, [], _) :- !.
visit_mst(G, Parent, [Vertex | Vertexes], PreorderTree) :-
   graph(G),
   append(PreorderTree, [arc(G, Parent, Vertex, _K)], NewPreorderTree),
   findall(C, vertex_previous(G, Vertex, C), Children),

   Children = [],
   visit_mst(G, Parent, Vertexes, NewPreorderTree),
   visit_mst(G, Parent, Vertexes, NewPreorderTree), !.
visit_mst(G, Parent, [Vertex | Vertexes], PreorderTree) :-
   graph(G),
   append(PreorderTree, [arc(G, Parent, Vertex, _K)], NewPreorderTree),
   findall(C, vertex_previous(G, Vertex, C), Children),
   Children \= [],
   visit_mst(G, Vertex, Children, NewPreorderTree),
   visit_mst(G, Parent, Vertexes, NewPreorderTree), !.
*/

% visit/3 - caso con assert di mst_arc

visit_mst(_, _, []) :- !.
visit_mst(G, Parent, [Vertex | Vertexes]) :-
   graph(G),
   arc(G, Parent, Vertex, K),
   not(mst_arc(G, Parent, Vertex, _)),
   assert(mst_arc(G, Parent, Vertex, K)),
   findall(C, vertex_previous(G, Vertex, C), Children), %trovo figli di vertex
   Children = [],
   visit_mst(G, Parent, Vertexes),
   visit_mst(G, Parent, Vertexes), !.
visit_mst(G, Parent, [Vertex | Vertexes]) :-
   graph(G),
   arc(G, Parent, Vertex, _K),
   findall(C, vertex_previous(G, Vertex, C), Children),
   Children = [],
   visit_mst(G, Parent, Vertexes),
   visit_mst(G, Parent, Vertexes), !.
visit_mst(G, Parent, [Vertex | Vertexes]) :-
   graph(G),
   arc(G, Parent, Vertex, _K),
   findall(C, vertex_previous(G, Vertex, C), Children),
   Children \= [],
   visit_mst(G, Vertex, Children),
   visit_mst(G, Parent, Vertexes), !.


% heap_insert_from_list/3

heap_insert_from_list(_, _, []).
heap_insert_from_list(G, H, [N | Ns]) :- %esiste un vertex key per V ed � infinito
   arg(3, N, V),
   arg(4, N, K),
   vertex_key(G, V, K1),
   K1 = inf,
   heap_insert(H, K, V),
   heap_insert_from_list(G, H, Ns), !.
heap_insert_from_list(G, H, [N | Ns]) :- %esiste un vertex key per V ma non inf
   arg(3, N, V),
   arg(4, N, _K),
   vertex_key(G, V, K1),
   K1 \= inf,
   heap_insert_from_list(G, H, Ns), !.




% new_heap/1

new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

% heap_head/3

heap_head(H, K, V) :- heap_entry(H, 0, K, V).

% delete_heap/1

delete_heap(H) :-
   retractall(heap_entry(H, _, _, _)),
   retract(heap(H, _)).

% heap_has_size/2

heap_has_size(H, S) :- heap(H, S).

% heap_empty/1

heap_empty(H) :- heap_has_size(H, 0).

% heap_not_empty/1

heap_not_empty(H) :- not(heap_empty(H)).

% heap_insert/3
% se K nuova � pi� piccola lo metto e faccio heapify, altrimenti nulla.

heap_insert(H, K, V) :- % controllare se V � dentro H
   heap(H, _S),
   heap_entry(H, _, KOld, V),
   KOld =< K, !.
heap_insert(H, K, V) :- % controllare se V � dentro H e nel caso aggiornarlo
   heap(H, S),
   heap_entry(H, _, KOld, V),
   KOld > K,
   retract(heap_entry(H, Pos, KOld, V)),
   assert(heap_entry(H, Pos, K, V)),
   heapify(H, S, Pos),!.
heap_insert(H, K, V) :- % V non � dentro H
   heap(H, S),
   NewSize is S + 1,
   retract(heap(H, S)),
   assert(heap(H, NewSize)),
   assert(heap_entry(H, S, K, V)),
   heapify(H, NewSize, S).

% heapify/3

heapify(_H, _S, I) :- I = 0, !. % caso base, l'heap contiene un solo elemento
heapify(H, S, I) :-          % passo induttivo
   heap(H, S),
   PosParent is floor((I - 1) / 2),
   heap_entry(H, I, K, V),
   heap_entry(H, PosParent, KP, VP),
   K < KP,
   assert(heap_entry(H, PosParent, K, V)),  % swap
   assert(heap_entry(H, I, KP, VP)),
   retract(heap_entry(H, I, K, V)),
   retract(heap_entry(H, PosParent, KP, VP)),
   heapify(H, S, PosParent), !.
heapify(H, S, I) :-
   heap(H, S),
   PosParent is floor((I - 1) / 2),
   heap_entry(H, I, K, V),
   heap_entry(H, PosParent, KP, VP),
   K = KP,
   V @< VP,
   assert(heap_entry(H, PosParent, K, V)),  % swap
   assert(heap_entry(H, I, KP, VP)),
   retract(heap_entry(H, I, K, V)),
   retract(heap_entry(H, PosParent, KP, VP)), !.
heapify(H, S, I) :-
   heap(H, S),
   PosParent is floor((I - 1) / 2),
   heap_entry(H, I, K, V),
   heap_entry(H, PosParent, KP, VP),
   K = KP,
   V @> VP, !.
heapify(H, S, I) :-
   heap(H, S),
   PosParent is floor((I - 1) / 2),
   heap_entry(H, I, K, _V),
   heap_entry(H, PosParent, KP, _VP),
   K > KP, !.


%%% vertex_previous(G, V, U) in cui U � parent

% heap_extract/3 - il predicato � vero quando la coppia K,V con K minima
% � rimossa dallo heap H. Da false se l'heap � vuoto.

heap_extract(H, K, V) :- % passo induttivo
   heap(H, S),
   LastPos is S - 1,
   heap_entry(H, 0, K, V),
   heap_entry(H, LastPos, K1, V1),
   assert(heap_entry(H, 0, K1, V1)),
   retract(heap_entry(H, 0, K, V)),
   retract(heap_entry(H, LastPos, K1, V1)),
   retract(heap(H, S)),
   assert(heap(H, LastPos)),
   fix_heap(H, LastPos, 0), !.

% fix_heap/3

fix_heap(_H, S, I) :-
   Left is 2 * I + 1,
   Left >= S, !.
fix_heap(H, S, I) :-
   Min is I,
   Left is 2 * I + 1,
   Right is 2 * I + 2,
   Right < S,
   heap_entry(H, Left, KL, _VL),
   heap_entry(H, Min, KM, VM),
   heap_entry(H, Right, KR, VR),
   min_list([KL, KM, KR], Smallest),
   Smallest = KR,
   assert(heap_entry(H, Min, KR, VR)),  % swap
   assert(heap_entry(H, Right, KM, VM)),
   retract(heap_entry(H, Right, KR, VR)),
   retract(heap_entry(H, Min, KM, VM)),
   fix_heap(H, S, Right), !.
fix_heap(H, S, I) :-
   Min is I,
   Left is 2 * I + 1,
   Right is 2 * I + 2,
   Left < S,
   heap_entry(H, Left, KL, VL),
   heap_entry(H, Min, KM, VM),
   heap_entry(H, Right, KR, _VR),
   min_list([KL, KR, KM], Smallest),
   Smallest = KL,
   assert(heap_entry(H, Min, KL, VL)),  % swap
   assert(heap_entry(H, Left, KM, VM)),
   retract(heap_entry(H, Left, KL, VL)),
   retract(heap_entry(H, Min, KM, VM)),
   fix_heap(H, S, Left), !.
fix_heap(H, S, I) :-
   Min is I,
   Left is 2 * I + 1,
   Right is 2 * I + 2,
   Right = S,
   heap_entry(H, Left, KL, VL),
   heap_entry(H, Min, KM, VM),
   KL < KM,
   assert(heap_entry(H, Min, KL, VL)),  % swap
   assert(heap_entry(H, Left, KM, VM)),
   retract(heap_entry(H, Left, KL, VL)),
   retract(heap_entry(H, Min, KM, VM)), !.
fix_heap(H, S, I) :-
   Min is I,
   Left is 2 * I + 1,
   Right is 2 * I + 2,
   Right = S,
   heap_entry(H, Left, KL, VL),
   heap_entry(H, Min, KM, VM),
   KM =< KL,
   VL @< VM,
   assert(heap_entry(H, Min, KL, VL)),  % swap
   assert(heap_entry(H, Left, KM, VM)),
   retract(heap_entry(H, Left, KL, VL)),
   retract(heap_entry(H, Min, KM, VM)), !.
fix_heap(H, S, I) :-
   Min is I,
   Left is 2 * I + 1,
   Right is 2 * I + 2,
   Right = S,
   heap_entry(H, Left, KL, VL),
   heap_entry(H, Min, KM, VM),
   KM =< KL,
   VL @> VM, !.
fix_heap(H, _S, I) :-
   Min is I,
   Left is 2 * I + 1,
   Right is 2 * I + 2,
   heap_entry(H, Left, KL, _VL),
   heap_entry(H, Min, KM, _VM),
   heap_entry(H, Right, KR, _VR),
   L = [KL, KM, KR],
   min_list(L, Smallest),
   Smallest = KM, !.

%list_heap

list_heap(H) :-
   heap(H, _),
   listing(heap_entry(H, _, _, _)).

% reset/0

reset() :-
   retractall(heap(_, _)),
   retractall(heap_entry(_, _, _, _)),
   retractall(vertex_key(_, _, _)),
   retractall(vertex_previous(_, _, _)),
   retractall(mst_arc(_, _, _, _)).


%%%% end of file -- mst.pl
