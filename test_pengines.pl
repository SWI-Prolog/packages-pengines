/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(test_pengines,
	  [ test_pengines/0
	  ]).

% setup paths to load relevant packages from development environment
:- asserta(user:file_search_path(foreign, '../http')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).

% Hack: auto-loading this does not work.
:- [library(charsio)].
:- [charsio:library(memfile)].

% the regular things we need for testing.
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

/** <module> Test suite for pengines
*/

test_pengines :-
    run_tests([ local_pengines,
		remote_pengines
	      ]).

% :- debug(pengine(_)).

:- begin_tests(local_pengines).

test(simple, Results = [a,b,c]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).
test(chunk, Results = [a,b,c]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, [chunk(2)]),
    assertion(no_more_pengines).
test(stop, Results = [a,b]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, [stop_after(2)]),
    assertion(no_more_pengines).
test(two, Sorted = [a,b,c,d,e,f]) :-
    pengine_create(
	[ src_text("p(a). p(b). p(c)."),
	  id(P1)
	]),
    pengine_create(
	[ src_text("p(d). p(e). p(f)."),
	  id(P2)
	]),
    collect(X, p(X), Results, [created([P1,P2])]),
    msort(Results, Sorted),
    assertion(no_more_pengines).

:- end_tests(local_pengines).

:- begin_tests(remote_pengines,
	       [ setup(start_pengine_server(_Port)),
		 cleanup(stop_pengine_server)
	       ]).

test(simple, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
	[ server(Server),
	  src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).
test(chunk, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
	[ server(Server),
	  src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, [chunk(2)]),
    assertion(no_more_pengines).
test(stop, Results = [a,b]) :-
    pengine_server(Server),
    pengine_create(
	[ server(Server),
	  src_text("p(a). p(b). p(c).")
	]),
    collect(X, p(X), Results, [stop_after(2)]),
    assertion(no_more_pengines).
test(two, Sorted = [a,b,c,d,e,f]) :-
    pengine_server(Server),
    pengine_create(
	[ server(Server),
	  src_text("p(a). p(b). p(c)."),
	  id(P1)
	]),
    pengine_create(
	[ server(Server),
	  src_text("p(d). p(e). p(f)."),
	  id(P2)
	]),
    collect(X, p(X), Results, [created([P1,P2])]),
    msort(Results, Sorted),
    assertion(no_more_pengines).
test(rpc, all(X == [1,2,3])) :-
    pengine_server(Server),
    pengine_rpc(Server,
		member(X, [1,2,3]),
		[]).
test(rpc, X == 1) :-
    pengine_server(Server),
    pengine_rpc(Server,
		member(X, [1,2,3]),
		[]), !.
test(rpc, fail) :-
    pengine_server(Server),
    pengine_rpc(Server,
		fail,
		[]), !.

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
%	  * created(Pengines)
%	  Passed to pengine_event_loop/2.
%
%	Remaining options are passed to pengine_ask/3.

collect(Template, Goal, Results, Options) :-
    (	select_option(stop_after(StopAfter), Options, Options1)
    ->	State = _{results:[], stop_after:StopAfter, options:AskOptions}
    ;	State = _{results:[], options:AskOptions},
	Options1 = Options
    ),
    (	select_option(created(Created), Options1, AskOptions)
    ->	EvOptions = [created(Created)]
    ;	EvOptions = [],
	AskOptions = Options1
    ),
    pengine_event_loop(collect_handler(Template, Goal, State), EvOptions),
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
    ->	pengine_next(Id, [])
    ;	pengine_destroy(Id)
    ).
collect_handler(_, _, _, failure(Id)) :-
    pengine_destroy(Id).

%%	no_more_pengines is semidet.
%
%	True if there are no more living pengines. Need to wait a little
%	because they die asynchronously.

no_more_pengines :-
    (	true
    ;	between(1, 10, _),
	sleep(0.01)
    ),
    \+ pengine:current_pengine(_,_,_,_,_,_), !.


		 /*******************************
		 *	    HTTP SERVER		*
		 *******************************/

:- http_handler(/, http_reply_from_files(web, []), [prefix]).

:- dynamic
	pengine_server_port/1.

pengine_server(URL) :-
	start_pengine_server(Port),
	format(atom(URL), 'http://localhost:~d', [Port]).

start_pengine_server(Port) :-
	pengine_server_port(Port), !.
start_pengine_server(Port) :-
	http_server(http_dispatch, [port(Port)]),
	asserta(pengine_server_port(Port)).

stop_pengine_server :-
	retract(pengine_server_port(Port)), !,
	http_stop_server(Port, []).
stop_pengine_server.
