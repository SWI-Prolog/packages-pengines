:- module(test_pengines,
	  [ test_pengines/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pengines)).

test_pengines :-
    run_tests([ local_pengines
	      ]).


:- begin_tests(local_pengines).

test(simple, Results = [a,b,c]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, all, []),
    assertion(no_more_pengines).
test(paging, Results = [b,a,c]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, all, [paging(2)]),
    assertion(no_more_pengines).

:- end_tests(local_pengines).


		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

%%	collect(+Template, :Goal, -Results, +StopAfter, +Options)
%
%	Collect answers from all pengines in Results. If StopAfter is an
%	integer,  collection  is  stopped  after  collecting  this  many
%	results.

collect(Template, Goal, Results, StopAfter, Options) :-
    State = results([], StopAfter, Options),
    pengine_event_loop(collect_handler(Template, Goal, State)),
    arg(1, State, R0),
    reverse(R0, Results).

collect_handler(Template, Goal, State, create(Id, _)) :-
    arg(3, State, Options),
    pengine_ask(Id, Goal, [template(Template)|Options]).
collect_handler(_, _, State, success(Id, Value, More)) :-
    arg(1, State, R0),
    append(Value, R0, R1),
    setarg(1, State, R1),
    (	arg(2, State, StopAfter),
	integer(StopAfter),
	length(R1, Collected),
	Collected >= StopAfter
    ->	pengine_destroy(Id)
    ;	More == true
    ->	pengine_next(Id)
    ;	pengine_destroy(Id)
    ).
collect_handler(_, _, _, failure(Id)) :-
    pengine_destroy(Id).

%%	no_more_pengines is semidet.
%
%	True if there are no more living pengines.

no_more_pengines :-
	\+ pengine:current_pengine(_,_,_,_).
