Kobril Christian 856448, Cicalla Eleonora 851649

% new_graph

Questo predicato inserisce un nuovo grafo nella base-dati Prolog.


% delete_graph

Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati Prolog.


% new_vertex

Aggiunge il vertice V nella base-dati Prolog


% graph_vertices

Questo predicato è vero quanto Vs è una lista contenente tutti i vertici di G.


% list_vertices

Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici del grafo G. 
 

% new_arc

Aggiunge un arco del grafo G alla base dati Prolog. N.B. è richiesto che il predicato che rappresenta gli archi, da aggiungere alla base-dati Prolog, sia arc(G, U, V, Weight).
 
% graph_arcs 

Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.

 
% vertex_neighbors

Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente gli archi, arc(G, V, N, W), che portano ai vertici N immediatamente raggiungibili da V.

%adjs

Questo predicato è vero quando V è un vertice di G e Vs è una lista contenente i vertici, vertex(G, V), ad esso adiacenti; si noti che in un grafo non diretto si devono inserire nella lista Vs tutti i vertici adiacenti.


%list_arcs

Questo predicato stampa alla console dell’interprete Prolog una lista degli archi del grafo G (è il simmetrico di list_vertices/1).


%list_graph

Questo predicato stampa alla console dell’interprete Prolog una lista dei vertici e degli archi del grafo G.


%read_graph

Questo predicato legge un “grafo” G, da un file FileName e lo inserisce nel data base di Prolog.


% save_graph

Questo è un predicato usato in read_graph per salvare il grafo letto nella memoria di prolog.

% write_graph

% create_rows
Questo è un predicato usato in write_graph che a partire da un grafo crea le tre colonne nelle quali andranno rispettivamente nodo di partenza, nodo di arrivo e peso dell'arco.


% set_inf

Questo è un predicato usato nel predicato mst_prim che imposta tutte le chiavi dei vertici di un grafo ad infinito.

% mst_prim

Questo predicato genera un mst prim. 


% recursive_mst_prim

Questo è un predicato usato alla fine del predicato mst_prim per invocare ricorsivamente le funzioni principali del predicato mst_prim senza dover ripetere le istruzioni che devono essere svolte solo una volta (set_inf, graph_vertices eccetera).


% find_min_arc

Questo è un predicato usato nel recursive_mst_prim che trova l'arco minimo.   


% mst_get

Questo predicato è vero quando PreorderTree è una lista degli archi del MST ordinata secondo un attraversamento preorder dello stesso, fatta rispetto al peso dell’arco.


% visit_mst

Questo è un predicato usato nella mst_get che si occupa di visitare l'mst dato un nodo Parent trovando e visitando i figli.


% heap_insert_from_list

Questo predicato è usato nella recursive_mst_prim e fa un inserimento nell'heap se esiste un vertex_key per V ed è in infinito.


% new_heap

Questo predicato inserisce un nuovo heap nella base-dati Prolog.


% heap_head

Questo predicato inserisce un nuovo heap nella base-dati Prolog.


% delete_heap

Rimuove tutto lo heap (incluse tutte le “entries”) dalla base-dati Prolog.


% heap_has_size

Questo predicato è vero quanto S è la dimensione corrente dello heap.


% heap_empty

Questo predicato è vero quando lo heap H non contiene elementi.

% heap_not_empty

Questo predicato è vero quando lo heap H contiene almeno un elemento.


% heap_insert

Il predicato insert/3 è vero quando l’elemento V è inserito nello heap H con chiave K. Naturalmente, lo heap H dovrà essere ristrutturato in modo da mantenere la proprietà che heap_head(H, HK, HV) sia vero per HK minimo e che la “heap property” sia mantenuta ad ogni nodo dello heap..


% heapify

Questo predicato ha il compito di assicurare il
rispetto della proprietà fondamentale degli Heap. Cioè, che il
valore di ogni nodo non sia inferiore di quello dei propri figli


% heap_extract

Il predicato extract/3 è vero quando la coppia K, V con K minima, è rimossa dallo heap H. Naturalmente, lo heap H dovrà essere ristrutturato in modo da mantenere la proprietà che heap_head(H, HK, HV) sia vero per HK minimo e che la “heap property” sia mantenuta ad ogni nodo dello heap.


% fix_heap

Questa è un heapifiy partendo dalla root


% list_heap

Il predicato richiama listing/1 per stampare sulla console Prolog lo stato interno dello heap.


% reset

Questo predicato cancella tutti gli heap, heap_entry, vertex_key, vertex_previous e mst_arc. 