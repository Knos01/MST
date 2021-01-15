# MST
Mst implementation by using HEAP


Per scaricare le modifiche digitare nella cartella del progetto, dal terminale: git pull

Per caricare le modifiche fatte, scrivere:
    1) git add .
    2) git commit -m "QUELLO CHE VUOI SCRIVERE"
    3) git push


IMPORTANTE: Prima di iniziare a lavorare, fare sempre git pull.

DOMANDE: Lasciamo il cut in list.... e graph....
list_vertices se non ci sono grafi stampa false?


:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
%tree_arc(G, V, U, Weight)
:- dynamic tree_arc/4.
