:- module(atom_to_list, [atom_to_list/2]).


atom_to_list(Atom, List) :-
    atom_to_term(Atom, List, []),  % [] To make sure no variables are present in the atom. 
    is_list(List).