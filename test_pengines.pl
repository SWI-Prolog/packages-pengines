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
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).
test(paging, Results = [a,b,c]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, [paging(2)]),
    assertion(no_more_pengines).
test(stop, Results = [a,b]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, [stop_after(2)]),
    assertion(no_more_pengines).
test(two, Sorted = [a,b,c,d,e,f]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    pengine_create(
	[ src_text("p(d). p(e). p(f).")
	]),
    collect(X, p(X), Results, []),
    msort(Results, Sorted),
    assertion(no_more_pengines).

:- end_tests(local_pengines).

:- begin_tests(remote_pengines).

test(simple, Results = [a,b,c]) :-
    pengine_create(
	[ server('http://localhost:4040'),
	  src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).

:- end_tests(remote_pengines).


		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

%%	collect(+Template, :Goal, -Results, +Options)
%
%	Collect answers from all pengines in Results.  Options:
%
%	  * stop_after(N)
%	  Stop collecting results after N answers

collect(Template, Goal, Results, Options) :-
    (	select_option(stop_after(StopAfter), Options, Options1)
    ->	State = _{results:[], stop_after:StopAfter, options:Options1}
    ;	State = _{results:[], options:Options}
    ),
    pengine_event_loop(collect_handler(Template, Goal, State)),
    Results = State.results.

collect_handler(Template, Goal, State, create(Id, _)) :-
    pengine_ask(Id, Goal, [template(Template)|State.options]).
collect_handler(_, _, State, success(Id, Values, More)) :-
    append(State.results, Values, R1),
    b_set_dict(results, State, R1),
    (	StopAfter = State.get(stop_after),
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
