:- module(term_to_json,
	  [ term_to_json/3,			% +Term, +Bindings, -Json 
	    term_to_json/2			% +Term, -Json 
	  ]).

:- use_module(library('http/json')).

%%	term_to_json(+Term, +Bindings, -JsonTerm) is det.
%%	term_to_json(+Term, -JsonTerm) is det.
%
%	Convert any general Prolog term into   a JSON term. Prolog lists
%	are  treated  in  a  special  way.  Also,  JSON  terms  are  not
%	converted. Mapping:
%
%	  * Variable: =|{"type":"var", "name":<string>}|=
%	  * Atom: =|{"type":"atom", "value":<string>}|=
%	  * Integer: =|{"type":"integer", "value":<integer>}|=
%	  * Float: =|{"type":"float", "value":<float>}|=
%	  * List of Name=Var pairs: JSON object
%	  * List: JSON array
%	  * compound: =|{"type":"compound", "functor":<string>, "args":<array>}|=
%
%	@param	Bindings is a list of Name=Var terms for variables that
%		get their name from the environment.

term_to_json(Term, Bindings, JSON) :-
	findall(X,
		(   maplist(bind_var, Bindings),
		    numbervars(Term, 0, _, [singletons(true)]),
		    to_json(Term, X)
		),
		[JSON]).

bind_var(Name=Var) :-
	(   var(Var)
	->  Var = '$VAR'(Name)
	;   true
	).

term_to_json(Term, JSON) :-
	findall(X,
		(   numbervars(Term, 0, _, [singletons(true)]),
		    to_json(Term, X)
		),
		[JSON]).


val_to_json(N=V, N=A) :-
    term_to_json(V,A).
    
    
to_json(Term, '_') :-
	var(Term), !.
to_json('$VAR'(Name), VarName) :- !,
	varname(Name, VarName).
to_json(@true, @true) :- !.
to_json(@false, @false) :- !.
to_json(@null, @null) :- !.
to_json(Term, Term) :-
	atom(Term), !.
to_json(Term, Value) :-
	integer(Term), !,
	(   Term >= -(2**31), Term < 2**31
	->  Value = Term
	;   atom_number(Value, Term)
	).
to_json(Term, Term) :-
	float(Term), !.
to_json(Term, json(JsonList)) :-
	is_pair_list(Term), !,
	maplist(val_to_json, Term, JsonList).
to_json(Term, JsonList) :-
	is_list(Term), !,
	maplist(to_json, Term, JsonList).
to_json(Term, Term) :-
	is_json_term(Term), !.
to_json(Term, json([functor=F, args=JsonArgs])) :-
	Term =.. [F|Args],
	maplist(to_json, Args, JsonArgs).

varname(Name, Name) :-
	atom(Name), !.
varname(I, Name) :-
	I < 26, !,
	Code is 0'A+I,
	char_code(Name, Code).
varname(I, Name) :-
	L is I mod 26,
	N is I // 26,
	varname(L, Name0),
	atomic_concat(Name0, N, Name).
	
is_pair_list([]).
is_pair_list([A=_|List]) :-
    atom(A),
    is_pair_list(List).


