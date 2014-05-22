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

:- module(pengines_io,
	  [ pengine_writeln/1,		% +Term
	    pengine_format/1,		% +Format
	    pengine_format/2,		% +Format, +Args

	    pengine_read/1,		% -Term

	    pengine_io_goal_expansion/2	% +Goal, -Expanded
	  ]).
:- use_module(library(pengines)).
:- use_module(library(http/html_write)).
:- html_meta send_html(html).

/** <module> Provide Prolog I/O for HTML clients

This module redefines some of  the   standard  Prolog  I/O predicates to
behave transparently for HTML  clients.  This   works  by  rewriting the
corresponding goals using goal_expansion/2 and use the new definition to
re-route  I/O  via  pengine_input/2  and   pengine_output/1.  A  pengine
application is prepared for using this module with the following code:

  ==
  :- pengine_application(myapp).
  :- use_module(myapp:library(pengines_io)).
  myapp:goal_expansion(In,Out) :-
	pengine_io_goal_expansion(In, Out).
  ==
*/


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

%%	pengine_writeln(+Term)
%
%	Emit Term as <div class=writeln>Term</div>.

pengine_writeln(Line) :-
	(   atomic(Line)
	->  String = Line
	;   term_string(Line, String)
	),
	send_html(div(class(writeln), String)).

%%	pengine_format(+Format) is det.
%%	pengine_format(+Format, +Args) is det.
%
%	As format/1,2. Emits a series  of   strings  with <br/> for each
%	newline encountered in the string.

pengine_format(Format) :-
	pengine_format(Format, []).
pengine_format(Format, Args) :-
	format(string(String), Format, Args),
	split_string(String, "\n", "", Lines),
	send_html(\lines(Lines)).


		 /*******************************
		 *	   PRINT MESSAGE	*
		 *******************************/

:- multifile user:message_hook/3.

%%	user:message_hook(+Term, +Kind, +Lines) is semidet.
%
%	Send output from print_message/2 to   the  pengine. Messages are
%	embedded in a <pre class=msg-Kind></pre> environment.

user:message_hook(_Term, Kind, Lines) :-
	Kind \== silent,
	pengine_self(_),
	atom_concat('msg-', Kind, Class),
	send_html(pre(class(Class), \message_lines(Lines))).

message_lines([]) --> [].
message_lines([nl|T]) --> !,
	html('\n'),			% we are in a <pre> environment
	message_lines(T).
message_lines([flush]) -->
	[].
message_lines([H|T]) --> !,
	html(H),
	message_lines(T).


		 /*******************************
		 *	       INPUT		*
		 *******************************/

pengine_read(Term) :-
	prompt(Prompt, Prompt),
	pengine_input(Prompt, Term).


		 /*******************************
		 *	       HTML		*
		 *******************************/

lines([]) --> [].
lines([H|T]) -->
	html(H),
	(   { T == [] }
	->  []
	;   html(br([])),
	    lines(T)
	).

%%	send_html(+HTML) is det.
%
%	Convert html//1 term into a string and send it to the client
%	using pengine_output/1.

send_html(HTML) :-
	phrase(html(HTML), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)),
	pengine_output(HTMlString).


		 /*******************************
		 *	    SANDBOXING		*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1.		% Goal

sandbox:safe_primitive(pengines_io:send_html(_)).
sandbox:safe_primitive(system:prompt(_,_)).


		 /*******************************
		 *	   REDEFINITION		*
		 *******************************/

pengine_io_goal_expansion(writeln(X),  pengine_writeln(X)).
pengine_io_goal_expansion(format(X),   pengine_format(X)).
pengine_io_goal_expansion(format(X,Y), pengine_format(X,Y)).
pengine_io_goal_expansion(read(X),     pengine_read(X)).

