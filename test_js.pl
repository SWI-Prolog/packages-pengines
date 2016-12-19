/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2015, VU University Amsterdam
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

% setup paths to load relevant packages from development environment
:- asserta(user:file_search_path(foreign, '../http')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(library, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(js, 'web/js')).

% Hack: auto-loading this does not work.
:- [library(charsio)].
:- [charsio:library(memfile)].

:- debug(pengine(delay)).

:- use_module(library(plunit)).
:- use_module(library(pengines)).
:- use_module(pengine_sandbox:library(pengines)).
:- use_module(library(process)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(http/jquery)).

:- pengine_application(swish).
:- use_module(swish:library(pengines_io)).
pengines:prepare_module(Module, swish, _Options) :-
    pengines_io:pengine_bind_io_to_html(Module).

% :- debug(http(request)).

test_js :-
    run_tests([ js_pengines
              ]).

:- dynamic
    test_config/1.

test_js(File, Atoms) :-
    test_js(File, null, Atoms).

test_js(File, Config, Atoms) :-
    pengine_server(Server),
    setup_call_cleanup(
        asserta(test_config(Config), Ref),
        setup_call_cleanup(
            test_script(File, Server, Script),
            ( process_create(path(phantomjs), [Script],
                             [ stdout(pipe(Input))
                             ]),
              read_string(Input, _, String),
              close(Input)
            ),
            delete_file(Script)),
        erase(Ref)),
    split_string(String, "\r\n", "\r\n", Lines),
    maplist(atom_string, Atoms, Lines).

test_script(File, Server, Script) :-
    tmp_file_stream(text, Script, Out),
    format(Out,
           'var page = require("webpage").create();\n\c
                var url = "~w/test_js/~w";\n\c
                page.onConsoleMessage = function(msg){\n\c
                  console.log(msg);\n\c
                };\n\c
                page.open(url, function (status) {\n\c
                  phantom.exit();\n\c
                });\n',
           [Server, File]),
    close(Out).

has_phantomjs :-
    absolute_file_name(path(phantomjs), _,
                       [ file_type(executable),
                         file_errors(fail),
                         access(execute)
                       ]).

phantomjs_version(Version) :-
    setup_call_cleanup(
        process_create(path(phantomjs), ['--version'],
                       [ stdout(pipe(Input))
                       ]),
        read_string(Input, _, String),
        close(Input)
    ),
    split_string(String, "", " \n\r\t", [Version]).

% POST sends empty request in this version.  See
% https://github.com/ariya/phantomjs/issues/14329
phantomjs_version_bad("2.1.1").

check_phantomjs :-
    has_phantomjs,
    !,
    phantomjs_version(Version),
    (   phantomjs_version_bad(Version)
    ->  print_message(
            warning,
            format('phantomjs version ~s is broken, \c
                        skipping JavaScript tests~n',
                   [Version]))
    ;   true
    ).
check_phantomjs :-
    print_message(
        warning,
        format('No phantomjs in $PATH, skipping JavaScript tests~n', [])).

:- public working_phantomjs/0.

working_phantomjs :-
    has_phantomjs,
    phantomjs_version(Version),
    \+ phantomjs_version_bad(Version).

:- initialization check_phantomjs.

:- begin_tests(js_pengines,
               [ setup(start_pengine_server(_Port)),
                 cleanup(stop_pengine_server),
                 condition(working_phantomjs)
               ]).

test(simple, Lines == [a,b,c,d,e,f,g]) :-
    test_js('simple.html', Lines).
test(notemplate, Lines == ['1','sunday','2','monday']) :-
    test_js('notemplate.html', Lines).
test(ask_syntax, Lines == [a]) :-
    test_js('ask_syntax.html', Lines).
test(sepresults, Lines == ['1', '2', '3', '4', a, b, c, d]) :-
    test_js('sepresults.html', Lines0),
    sort(Lines0, Lines).
test(json_s, Lines == ['1', 'a', '\'a b\'', '"s"', 'c(a)']) :-
    test_js('test_json_s.html', _{}, Lines).
test(json_s, Lines == ['1', 'a', '\'a b\'', '"s"', 'c(a)']) :-
    test_js('test_json_s.html', _{askOptions:_{chunk:3}}, Lines).
test(json_s, Lines == ['1', 'a', '\'a b\'', '"s"', 'c(a)']) :-
    test_js('test_json_s.html', _{ask:"data(A)"}, Lines).
test(json_html, Lines == [ 'A',
                           '<span class="pl-int">1</span>',
                           'A',
                           '<span class="pl-atom">a</span>',
                           'A',
                           '<span class="pl-quoted-atom">\'a b\'</span>',
                           'A',
                           '<span class="pl-string">"s"</span>',
                           'A',
                           '<span class="pl-compound"><span class="pl-functor">c</span>(<span class="pl-atom">a</span>)</span>'
                         ]) :-
    test_js('test_json_html.html', Lines).

:- end_tests(js_pengines).



                 /*******************************
                 *          HTTP SERVER         *
                 *******************************/

:- http_handler(/, http_reply_from_files(web, []), [prefix]).
:- http_handler('/test_js/config.js', page_config, []).

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

page_config(_Request) :-
    test_config(Config),
    format('Content-type: text/javascript~n~n'),
    format('var config = '),
    json_write_dict(current_output, Config),
    format(';\n').
