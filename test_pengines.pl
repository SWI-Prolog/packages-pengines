/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(test_pengines,
          [ test_pengines/0,
            pengine_server/0                    % start server
          ]).
:- include(test_local).

:- debug(pengine(delay)).
% run pengine server for remote tests in a separate process.
% :- debug(pengine(external_server)).

% the regular things we need for testing.
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(pengines_sandbox)).
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
test(chunk2, Results = [a,b,c]) :-
    pengine_create(
        [ src_text("p(a). p(b). p(c).")
        ]),
    collect_state(X, p(X), State, [chunk(2), next(2)]),
    Results = State.results,
    assertion(State.replies = 2),
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
        [ alias(pippi),
          id(Id)
        ]),
    pengine_property(Id, alias(Name)),
    assertion(( pengine_property(Id, thread(Thread)),
                (   thread_property(Thread, alias(ThreadAlias))
                ->  ThreadAlias \== Name
                ;   true
                ))),
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
                []),
    !,
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
test(ask_simple_no_template, Results = [p(a),p(b),p(c)]) :-
    pengine_server(Server),
    pengine_create(
        [ ask(p(_X)),
          server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    collect(Results, []),
    assertion(no_more_pengines).
test(rpc_nested, Xs == [1,2,3]) :-
    pengine_server(Server),
    findall(X, pengine_rpc(Server,
                           pengine_rpc(Server,
                                       member(X, [1,2,3]),
                                       []),
                           []),
            Xs),
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
                 *          APPLICATION         *
                 *******************************/

:- pengine_application(papp).
:- use_module(papp:library(pengines)).

papp:p1(a).
papp:p1(b).
papp:p1(c).


                 /*******************************
                 *           UTILITIES          *
                 *******************************/

%!  collect(+Template, :Goal, -Results, +Options)
%
%   Collect answers from all pengines in Results.  Options:
%
%     * stop_after(N)
%     Stop collecting results after N answers
%     * next(N)
%     Passed to pengine_next/2
%
%   Other options are passed to pengine_ask/3.

collect(Results, Options) :-
    collect(-, -, Results, Options).

collect(Template, Goal, Results, Options) :-
    collect_state(Template, Goal, State, Options),
    Results = State.results.

collect_state(Template, Goal, State, Options) :-
    partition(next_option, Options, NextOptions, Options1),
    partition(state_option, Options1, StateOptions, AskOptions),
    dict_create(State, state,
                [ results([]),
                  replies(0),
                  options(_{ask:AskOptions, next:NextOptions})
                | StateOptions
                ]),
    pengine_event_loop(collect_handler(Template, Goal, State), []).

state_option(stop_after(_)).
next_option(next(_)).

collect_handler(Template, Goal, State, create(Id, _)) :-
    Goal \== (-),
    pengine_ask(Id, Goal, [template(Template)|State.options.ask]).
collect_handler(_, _, State, success(Id, Values, More)) :-
    append(State.results, Values, R1),
    b_set_dict(results, State, R1),
    Replies1 is State.replies+1,
    b_set_dict(replies, State, Replies1),
    (   StopAfter = State.get(stop_after),
        length(R1, Collected),
        Collected >= StopAfter
    ->  pengine_destroy(Id)
    ;   More == true
    ->  pengine_next(Id, State.options.next)
    ;   true
    ).

%!  no_more_pengines is semidet.
%
%   True if there are no more living pengines. Need to wait a little
%   because they die asynchronously.

no_more_pengines :-
    (   true
    ;   between(1, 10, _),
        sleep(0.01)
    ),
    \+ pengines:current_pengine(_,_,_,_,_,_),
    \+ pengines:child(_,_),
    !.


                 /*******************************
                 *          HTTP SERVER         *
                 *******************************/

:- pengine_sandbox:use_module(library(pengines)).
:- http_handler(/, http_reply_from_files(web, []), [prefix]).

:- dynamic
    pengine_server_port/1.

pengine_server(URL) :-
    debugging(pengine(external_server)),
    !,
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
    pengine_server_pid(PID),
    !,
    process_kill(PID, hup),
    process_wait(PID, _Status).             % getting status 2??
%       assertion(Status == exit(0)).
stop_pengine_server :-
    retract(pengine_server_port(Port)),
    !,
    http_stop_server(Port, []).
stop_pengine_server.


                 /*******************************
                 *       EXTERNAL SERVER                *
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
