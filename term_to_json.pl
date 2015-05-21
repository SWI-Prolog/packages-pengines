/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, Torbjörn Lager,
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(term_to_json,
	  [ term_to_json/3,			% +Term, +Bindings, -Json
	    term_to_json/2			% +Term, -Json
	  ]).
:- use_module(library(apply)).
:- use_module(library(error)).

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
%	  * List: JSON array
%	  * Dict: a JSON object. Values are processed recursively.
%           (the tag is ignored)
%	  * json([Key=Value, ...]): a JSON object Values are processed
%	    recursively.
%	  * compound: =|{"type":"compound", "functor":<string>, "args":<array>}|=
%
%	@param	Bindings is a list of Name=Var terms for variables that
%		get their name from the environment.

term_to_json(Term, JSON) :-
	term_to_json(Term, [], JSON).
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

to_json(Term, '_') :-
	var(Term), !.
to_json(@(Symbol), Symbol) :-			% compatibility
	atom(Symbol),
	json_symbol(Symbol), !.
to_json(Term, Term) :-
	atom(Term), !.				% interpreted as a string
to_json(Term, Term) :-
	string(Term), !.
to_json(Term, Value) :-
	integer(Term), !,
	(   Term >= -(2**31), Term < 2**31
	->  Value = Term
	;   atom_number(Value, Term)
	).
to_json(Term, Term) :-
	float(Term), !.
to_json(Term, JsonList) :-
	is_list(Term), !,
	maplist(to_json, Term, JsonList).
to_json(json(Pairs0), Term) :-
	must_be(list, Pairs0),
	maplist(pair_value_to_json_ex, Pairs0, Pairs),
	dict_pairs(Term, json, Pairs).
to_json(Term0, Term) :-
	is_dict(Term0), !,
	dict_pairs(Term0, Tag, Pairs0),
	maplist(pair_value_to_json, Pairs0, Pairs),
	dict_pairs(Term, Tag, Pairs).
to_json('$VAR'(Name), VarName) :- !,
	format(string(VarName), '~W', ['$VAR'(Name), [numbervars(true)]]).
to_json(Term, json{functor:F, args:JsonArgs}) :-
	Term =.. [F|Args],
	maplist(to_json, Args, JsonArgs).

json_symbol(null).
json_symbol(true).
json_symbol(false).

pair_value_to_json(Key-Value0, Key-Value) :-
	to_json(Value0, Value).

pair_value_to_json_ex(Key=Value0, Key-Value) :-
	(atom(Key) ; integer(Key)), !,
	to_json(Value0, Value).
pair_value_to_json_ex(Elem, _) :-
	domain_error(json_key_value, Elem).

