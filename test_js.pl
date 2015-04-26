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

:- public has_phantomjs/0.

has_phantomjs :-
	absolute_file_name(path(phantomjs), _,
			   [ file_type(executable),
			     file_errors(fail),
			     access(execute)
			   ]).

check_phantomjs :-
	has_phantomjs, !.
check_phantomjs :-
	format(user_error,
	       'No phantomjs in $PATH, skipping JavaScript tests~n', []).

:- initialization check_phantomjs.

:- begin_tests(js_pengines,
	       [ setup(start_pengine_server(_Port)),
		 cleanup(stop_pengine_server),
		 condition(has_phantomjs)
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
		 *	    HTTP SERVER		*
		 *******************************/

:- http_handler(/, http_reply_from_files(web, []), [prefix]).
:- http_handler('/test_js/config.js', page_config, []).

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

page_config(_Request) :-
	test_config(Config),
	format('Content-type: text/javascript~n~n'),
	format('var config = '),
	json_write_dict(current_output, Config),
	format(';\n').
