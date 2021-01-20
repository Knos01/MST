%%%% -*- Mode: Prolog -*-

:- dynamic graph/1.
:- dynamic graph/2.
:- dynamic vertex/1.
:- dynamic vertex/2.
:- dynamic arc/3.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap/1.
:- dynamic heap_entry/4.
:- dynamic visited/2.
:- dynamic previous/3.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic node/3.
:- dynamic row/3.

% new_graph

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

% delete_graph

delete_graph(G) :-
   retractall(arc(G, _, _, _)),
   retractall(vertex(G, _)),
   retract(graph(G)).

% new_vertex

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- assert(vertex(G, V)), !.

% graph_vertices

graph_vertices(G, Vs) :- findall(V, vertex(G, V), Vs).

% list_vertices

list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)).

% new_arc

new_arc(G, U, V, Weight) :- arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :-
   assert(arc(G, U, V, Weight)),
   assert(arc(G, V, U, Weight)),
   !.
new_arc(G, U, V) :- new_arc(G, U, V, 1).

% graph_arcs

graph_arcs(G, Es) :- findall(arc(G, U, V, W), arc(G, U, V, W), Es).

% vertex_neighbors

vertex_neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns).

% adjs

adjs(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, N), arc(G, V, N, _), Vs).

% list_arcs

list_arcs(G) :-
    graph(G),
    listing(arc(G, _, _, _)).

% list_graph

list_graph(G) :-
    graph(G),
    list_vertices(G),
    list_arcs(G), !.

% read_graph

read_graph(G, FileName) :-
    new_graph(G),
    csv_read_file(FileName, Rows, [separator(0'\t)]),
                                  save_graph(G, Rows).
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

list_rows():- listing(row(_, _, _)).

% write_graph
%
write_graph(G, FileName) :- write_graph(G, FileName, graph).
write_graph(G, FileName, Type):-
    Type = graph,
    findall(row(U, V, W), arc(G, U, V, W), Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]), !. % file location?
write_graph(G, FileName, Type):-
    Type = edges,
    create_rows(G),
    findall(row(U, V, W), row(U, V, W), Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]),
    retractall(row(_, _, _)).

create_rows([]).
create_rows([B | Bs]) :-
    arg(2, B, U),
    arg(3, B, V),
    arg(4, B, W),
    Term =.. [row, U, V, W],
    assert(Term),
    create_rows(Bs).

%%%%%%%%%%%%%%%
%%%%%%MST%%%%%%
%%%%%%%%%%%%%%%

% set_inf

set_inf(_, []).
set_inf(G, [V | Vs]):-
    assert(vertex_key(G, V, inf)),
    set_inf(G, Vs).

%mst_prim

mst_prim(G, Source) :-
   graph_vertices(G, V),
   set_inf(G, V),
   retract(vertex_key(G, Source, inf)),
   assert(vertex_key(G, Source, 0)),
   new_heap(p),
   heap_insert(Source),
   vertex_neighbors(G, Source, N),
   heap_n(G, N).

%suorce
%neighbours
%metto i neighbours nell'heap
%tolgo la testa (heap extract)
%vertex_key
%vertex_previous
%


heap_n(_, []).
heap_n(G, [N | Ns]) :-
    nth(4, N, W),
    nth(3, N, U),
    heap_insert(p, W, U),
    heap_n(G, Ns).

%mst_get

%mst_get(G, Source, PreorderTree).

%%%%%%%%%%%%%%%
%%%%MiniHeap%%%
%%%%%%%%%%%%%%%

%new_heap

new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

%delete_heap

delete_heap(H) :- retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).

%heap_has_size

heap_has_size(H, S) :- heap(H, S).

%heap_empty

heap_empty(H) :- heap_has_size(H, 0).

%heap_not_empty

heap_not_empty(H) :- not(heap_empty(H)).

%heap_head

heap_head(H, K, V) :-  heap_entry(H, 1, K, V).

%heap_insert

heap_insert(H, K, V) :-
    heap(H, S),
    retract(heap(H, _)),
    Sum is S + 1, new_heap(H),
    assert(heap(H, Sum)),
    assert(heap_entry(H,  Sum, K, V)),
    assert(node(H, K, V)), heapify(H, Sum).

%heapify



%heap_extract

heap_extract(H, K, V) :-
    retract(node(H, K, V)),
    delete_heap(H),
    new_heap(H),
    findall(K1, node(H, K1, V1), Lk),
    findall(V1, node(H, K1, V1), Lv),
    add2(H, Lk, Lv).

add2(_H, [], []) :- !.
add2(H, [K|Ks], [V|Vs]) :- heap_insert(H, K, V), add2(H, Ks, Vs).

%modify key

modify_key(H, NewKey, OldKey, V) :-
    heap_entry(H, _, OldKey, V),
    retract(node(H, OldKey, V)),
    assert(node(H, NewKey, V )),
    delete_heap(H), new_heap(H),
    findall(K1, node(H, K1, V1), Lk),
    findall(V1, node(H, K1, V1), Lv),
    add(H, Lk, Lv), !.


add(_H, [], []) :- !.
add(H, [K|Ks], [V|Vs]) :-
    add(H, K, V),
    add3(H, Ks, Vs).
add3(H, K, V) :-
    heap(H, S),
    retract(heap(H, _)),
    Sum is S + 1, new_heap(H),
    assert(heap_entry(H,  Sum, K, V)), heapify(H, Sum).

%list_heap

list_heap(H) :- listing(heap_entry(H, _, _, _)).
