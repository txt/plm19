
longest --> weight, sort, first.       
weight -->  maplist(long).                                                           
long(Word, N / Word) :- atom_length(Word,M), N is -1*M.               

first([ _/X | _ ], X).                                              

 :- longest([names,age,shoesize], X), print(X).   
