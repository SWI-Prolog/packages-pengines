/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2025, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- module(test_js,
          [ test_js/0
          ]).

/** <module> Test JavaScript Pengines interaction

This module scripts the browser and Node.js interaction with a Pengines server.
The set of test cases is based on BDD conventions and is ran by the Mocha test
runner. The browser test environment uses headless Chromium driven through
the debug protocol using the Puppeteer NPM package. The tests were previously
run using SlimerJS (fragile) and before that using PhantomJS (not maintained).

To run this code, install all required dependencies using NPM:

    % npm install
*/

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

:- use_module(library(plunit)).
:- use_module(library(pengines)).
:- use_module(library(pengines_sandbox)).
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(library(process)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(json)).
:- use_module(library(http/jquery)).

:- pengine_application(swish).
:- use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :-
    pengines_io:pengine_bind_io_to_html(Module).

% Necessary for browser tests.
:- set_setting_default(http:cors, [*]).

% :- debug(http(request)).

test_js :-
    run_tests([ js_pengines
              ]).

run_test_script(Script, Status):-
    pengine_server_port(Port),
    process_create(path(node), [Script, Port],
        [stdin(std), stdout(std), stderr(std), process(PID)]),
    process_wait(PID, Status).

:- begin_tests(js_pengines,
               [ setup(start_pengine_server(_Port)),
                 cleanup(stop_pengine_server)
               ]).

test(browser):-
    run_test_script('test_js/browser.js', Status),
    assertion(Status == exit(0)).

test(node):-
    run_test_script('test_js/node.js', Status),
    assertion(Status == exit(0)).

:- end_tests(js_pengines).



                 /*******************************
                 *          HTTP SERVER         *
                 *******************************/

:- http_handler(/, http_reply_from_files(web, []), [prefix]).

:- dynamic
    pengine_server_port/1.

pengine_server(URL) :-
    start_pengine_server(Port),
    format(atom(URL), 'http://localhost:~d', [Port]).

start_pengine_server(Port) :-
    pengine_server_port(Port),
    !.
start_pengine_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    asserta(pengine_server_port(Port)).

stop_pengine_server :-
    retract(pengine_server_port(Port)),
    !,
    http_stop_server(Port, []).
stop_pengine_server.
