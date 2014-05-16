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
	  [ test_pengines/0,
	    pengine_server/0			% start server
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

:- debug(pengine(delay)).
% run pengine server for remote tests in a separate process.
% :- debug(pengine(external_server)).

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
		remote_pengines,
		application
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
	[ src_text("p(a). p(b). p(c).")
	]),
    pengine_create(
	[ src_text("p(d). p(e). p(f).")
	]),
    collect(X, p(X), Results, []),
    msort(Results, Sorted),
    assertion(no_more_pengines).
test(alias, Name == pippi) :-
    pengine_create(
	[ name(pippi),
	  id(Id)
	]),
    pengine_property(Id, alias(Name)),
    collect(_, fail, Results, []),
    assertion(Results == []),
    assertion(no_more_pengines).
test(ask_simple, Results = [a,b,c]) :-
    pengine_create(
	[ ask(p(X)),
	  template(X),
	  src_text("p(a). p(b). p(c).")
	]),
    collect(Results, []),
    assertion(no_more_pengines).
test(ask_fail, Results = []) :-
    pengine_create(
	[ ask(p(X)),
	  template(X),
	  src_text("p(_) :- fail.")
	]),
    collect(Results, []),
    assertion(no_more_pengines).

:- end_tests(local_pengines).

:- begin_tests(remote_pengines,
	       [ setup(pengine_server(_URL)),
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
	  src_text("p(a). p(b). p(c).")
	]),
    pengine_create(
	[ server(Server),
	  src_text("p(d). p(e). p(f).")
	]),
    collect(X, p(X), Results, []),
    msort(Results, Sorted),
    assertion(no_more_pengines).
test(rpc_det, Xs == [1]) :-
    pengine_server(Server),
    findall(X, pengine_rpc(Server,
			   X = 1,
			   []),
	    Xs),
    assertion(no_more_pengines).
test(rpc_all, Xs == [1,2,3]) :-
    pengine_server(Server),
    findall(X, pengine_rpc(Server,
			   member(X, [1,2,3]),
			   []),
	    Xs),
    assertion(no_more_pengines).
test(rpc_first, X == 1) :-
    pengine_server(Server),
    pengine_rpc(Server,
		member(X, [1,2,3]),
		[]), !,
    assertion(no_more_pengines).
test(rpc_fail, true) :-
    pengine_server(Server),
    \+ pengine_rpc(Server,
		   fail,
		   []),
    assertion(no_more_pengines).
test(pengine_and_rpc, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
	[ server(Server),
	  src_text("p(a). p(b). p(c).")
	]),
    findall(R, pengine_rpc(Server, member(R, [1,2,3]), []), Rs),
    assertion(Rs == [1,2,3]),
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).
test(simple_app, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
	[ server(Server),
	  application(papp)
	]),
    collect(X, p1(X), Results, []),
    assertion(no_more_pengines).
test(noapp, error(existence_error(pengine_application, nopapp))) :-
    pengine_server(Server),
    pengine_create(
	[ server(Server),
	  application(nopapp)
	]),
    collect(X, p1(X), _Results, []),
    assertion(no_more_pengines).
test(ask_simple, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
	[ ask(p(X)),
	  template(X),
	  server(Server),
	  src_text("p(a). p(b). p(c).")
	]),
    collect(Results, []),
    assertion(no_more_pengines).

:- end_tests(remote_pengines).

:- begin_tests(application).

test(simple, Results = [a,b,c]) :-
    pengine_create(
	[ application(papp)
	]),
    collect(X, p1(X), Results, []),
    assertion(no_more_pengines).
test(self, true) :-
    pengine_create([ application(papp)
		   ]),
    collect(X, pengine_self(X), Results, []),
    Results = [Self],
    assertion(atom(Self)),
    assertion(no_more_pengines).
test(noapp, error(existence_error(pengine_application, nopapp))) :-
    pengine_create(
	[ application(nopapp)
	]),
    collect(X, p1(X), _Results, []),
    assertion(no_more_pengines).

:- end_tests(application).


		 /*******************************
		 *	    APPLICATION		*
		 *******************************/

:- pengine_application(papp).
:- use_module(papp:library(pengines)).

papp:p1(a).
papp:p1(b).
papp:p1(c).


		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

%%	collect(+Template, :Goal, -Results, +Options)
%
%	Collect answers from all pengines in Results.  Options:
%
%	  * stop_after(N)
%	  Stop collecting results after N answers

collect(Results, Options) :-
    collect(-, -, Results, Options).

collect(Template, Goal, Results, Options) :-
    (	select_option(stop_after(StopAfter), Options, Options1)
    ->	State = _{results:[], stop_after:StopAfter, options:Options1}
    ;	State = _{results:[], options:Options}
    ),
    pengine_event_loop(collect_handler(Template, Goal, State), []),
    Results = State.results.

collect_handler(Template, Goal, State, create(Id, _)) :-
    Goal \== (-),
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
    ;	true
    ).

%%	no_more_pengines is semidet.
%
%	True if there are no more living pengines. Need to wait a little
%	because they die asynchronously.

no_more_pengines :-
    (	true
    ;	between(1, 10, _),
	sleep(0.01)
    ),
    \+ pengine:current_pengine(_,_,_,_,_,_),
    \+ pengine:child(_,_), !.


		 /*******************************
		 *	    HTTP SERVER		*
		 *******************************/

:- http_handler(/, http_reply_from_files(web, []), [prefix]).

:- dynamic
	pengine_server_port/1.

pengine_server(URL) :-
	debugging(pengine(external_server)), !,
	start_external_server(URL).
pengine_server(URL) :-
	local_server(URL).

local_server(URL) :-
	(   pengine_server_port(Port)
	->  true
	;   http_server(http_dispatch, [port(Port)]),
	    asserta(pengine_server_port(Port))
	),
	format(atom(URL), 'http://localhost:~d', [Port]).

stop_pengine_server :-
	pengine_server_pid(PID), !,
	process_kill(PID, hup),
	process_wait(PID, _Status).		% getting status 2??
%	assertion(Status == exit(0)).
stop_pengine_server :-
	retract(pengine_server_port(Port)), !,
	http_stop_server(Port, []).
stop_pengine_server.


		 /*******************************
		 *	 EXTERNAL SERVER		*
		 *******************************/

:- dynamic pengine_server_pid/1.

start_external_server(URL) :-
	current_prolog_flag(executable, SWIPL),
	process_create(SWIPL,
		       [ '-q', '-f', 'test_pengines.pl',
			 '-g', 'pengine_server'
		       ],
		       [ stdout(pipe(Out)),
			 process(PID)
		       ]),
	read_line_to_string(Out, URL),
	assertion(string_concat('http://', _, URL)),
	asserta(pengine_server_pid(PID)),
	on_signal(hup, _, hangup).

hangup(_Signal) :-
	format(user_error, 'Got hangup~n', []),
	thread_send_message(main, done).

pengine_server :-
	local_server(URL),
	writeln(URL),
	thread_get_message(Done),
	format(user_error, 'Got ~p', [Done]).
