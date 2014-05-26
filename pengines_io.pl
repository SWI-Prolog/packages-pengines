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
	    pengine_nl/0,
	    pengine_format/1,		% +Format
	    pengine_format/2,		% +Format, +Args

	    pengine_write_term/2,	% +Term, +Options
	    pengine_write/1,		% +Term
	    pengine_writeq/1,		% +Term
	    pengine_display/1,		% +Term
	    pengine_print/1,		% +Term
	    pengine_write_canonical/1,	% +Term

	    pengine_listing/0,
	    pengine_listing/1,		% +Spec

	    pengine_read/1,		% -Term

	    pengine_io_goal_expansion/2	% +Goal, -Expanded
	  ]).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
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

:- setting(write_options, list(any), [max_depth(1000)],
	   'Additional options for stringifying Prolog results').


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/

%%	pengine_writeln(+Term)
%
%	Emit Term as <div class=writeln>Term</div>.

pengine_writeln(Line) :-
	atomic(Line), !,
	send_html(div(class(writeln), Line)).
pengine_writeln(Term) :-
	pengine_module(Module),
	send_html(div(class(writeln),
		      \term(Term,
			    [ quoted(true),
			      module(Module)
			    ]))).


%%	pengine_nl
%
%	Emit a <br/> to the pengine.

pengine_nl :-
	send_html(br([])).


%%	pengine_write_term(+Term, +Options)
%
%	Writes term as <span class=Class>Term</span>. In addition to the
%	options of write_term/2, these options are processed:
%
%	  - class(+Class)
%	    Specifies the class of the element.  Default is =write=.

pengine_write_term(Term, Options) :-
	option(class(Class), Options, write),
	pengine_module(Module),
	send_html(span(class(Class), \term(Term,[module(Module)|Options]))).

%%	pengine_write(+Term) is det.
%%	pengine_writeq(+Term) is det.
%%	pengine_display(+Term) is det.
%%	pengine_print(+Term) is det.
%%	pengine_write_canonical(+Term) is det.
%
%	Redirect the corresponding Prolog output predicates.

pengine_write(Term) :-
	pengine_write_term(Term, []).
pengine_writeq(Term) :-
	pengine_write_term(Term, [quoted(true), numbervars(true)]).
pengine_display(Term) :-
	pengine_write_term(Term, [quoted(true)]).
pengine_print(Term) :-
	current_prolog_flag(print_write_options, Options),
	pengine_write_term(Term, Options).
pengine_write_canonical(Term) :-
	with_output_to(string(String), write_canonical(Term)),
	send_html(span(class([write, cononical]), String)).

%%	pengine_format(+Format) is det.
%%	pengine_format(+Format, +Args) is det.
%
%	As format/1,2. Emits a series  of   strings  with <br/> for each
%	newline encountered in the string.
%
%	@tbd: handle ~w, ~q, etc using term//2.  How can we do that??

pengine_format(Format) :-
	pengine_format(Format, []).
pengine_format(Format, Args) :-
	format(string(String), Format, Args),
	split_string(String, "\n", "", Lines),
	send_html(\lines(Lines)).


		 /*******************************
		 *	      LISTING		*
		 *******************************/

%%	pengine_listing
%%	pengine_listing(+Spec)
%
%	List the content of the current pengine or a specified predicate
%	in the pengine. Does not allow   for listing outside the pengine
%	module.

pengine_listing :-
	pengine_listing(_).

pengine_listing(Spec) :-
	(   nonvar(Spec),
	    Spec = M:_
	->  permission_error(listing, module, M)
	;   true
	),
	pengine_self(Module),
	with_output_to(string(String), listing(Module:Spec)),
	send_html(pre(class(listing), String)).


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


%%	pengine_module(-Module) is det.
%
%	Module (used for resolving operators).

pengine_module(Module) :-
	pengine_self(Pengine), !,
	pengine_property(Pengine, module(Module)).
pengine_module(user).

		 /*******************************
		 *	  OUTPUT FORMAT		*
		 *******************************/

%%	pengines:event_to_json(+Event, -JSON, +Format) is semidet.
%
%	Provide additional translations for  Prolog   terms  to  output.
%	Defines formats are:
%
%	  * 'json-s'
%	  _Simple_ or _string_ format: Prolog terms are sent using
%	  quoted write.
%	  * 'json-html'
%	  Serialize responses as HTML string.  This is intended for
%	  applications that emulate the Prolog toplevel.  This format
%	  carries the following data:
%
%	    - data
%	      List if answers, where each answer is an object with
%	      - variables
%	        Array of objects, each describing a variable.  These
%	        objects contain these fields:
%	        - variables: Array of strings holding variable names
%	        - value: HTML-ified value of the variables
%	        - substitutions: Array of objects for substitutions
%	          that break cycles holding:
%		  - var: Name of the inserted variable
%		  - value: HTML-ified value
%	      - residuals
%	        Array of strings representing HTML-ified residual goals.

:- multifile
	pengine:event_to_json/3.

/* JSON-S */

pengine:event_to_json(success(ID, Bindings0, More),
		      json([event=success, id=ID, data=Bindings, more= @(More)]),
		       'json-s') :- !,
	maplist(solution_to_json(ID), Bindings0, Bindings).
pengine:event_to_json(output(ID, Term),
		      json([event=output, id=ID, data=Data]), 'json-s') :- !,
	(   atomic(Term)
	->  Data = Term
	;   term_string(Term, Data)
	).

solution_to_json(Pengine, BindingsIn, json(BindingsOut)) :-
    maplist(term_string_value(Pengine), BindingsIn, BindingsOut).

term_string_value(Pengine, N=V, N=A) :-
	with_output_to(string(A),
		       write_term(V,
				  [ module(Pengine),
				    quoted(true)
				  ])).

/* JSON-HTML */

pengine:event_to_json(success(ID, Answers0, More),
		      json{event:success,
			   id:ID,
			   data:Answers,
			   more:More
			  },
		      'json-html') :- !,
	maplist(map_answer(ID), Answers0, Answers).
pengine:event_to_json(output(ID, Term),
		      json{event:output, id:ID, data:Data}, 'json-html') :- !,
	(   atomic(Term)
	->  Data = Term
	;   term_html_string(ID, Term, Data)
	).

map_answer(ID, Bindings0, Answer) :-
	prolog:translate_bindings(Bindings0, Bindings1, Residuals),
	maplist(binding_to_html(ID), Bindings1, VarBindings),
	(   Residuals == []
	->  Answer = json{variables:VarBindings}
	;   maplist(term_html_string(ID), Residuals, ResHTML),
	    Answer = json{variables:VarBindings, residuals:ResHTML}
	).


binding_to_html(ID, binding(Vars,Term,Substitutions), JSON) :-
	JSON0 = json{variables:Vars, value:HTMLString},
	term_html_string(ID, Term, HTMLString),
	(   Substitutions == []
	->  JSON = JSON0
	;   maplist(subst_to_html(ID), Substitutions, HTMLSubst),
	    JSON = JSON0.put(substitutions, HTMLSubst)
	).

term_html_string(Pengine, Term, HTMLString) :-
	setting(write_options, Options),
%	pengine_property(Pengine, module(Module)),
	phrase(term(Term, [ quoted(true),
			    numbervars(true),
			    module(Pengine)
			  | Options
			  ]), Tokens),
	with_output_to(string(HTMLString), print_html(Tokens)).

subst_to_html(ID, '$VAR'(Name)=Value, json{var:Name, value:HTMLString}) :- !,
	term_html_string(ID, Value, HTMLString).
subst_to_html(_, Term, _) :-
	assertion(Term = '$VAR'(_)).


		 /*******************************
		 *	    SANDBOXING		*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1.		% Goal

sandbox:safe_primitive(pengines_io:send_html(_)).
sandbox:safe_primitive(pengines_io:pengine_listing(_)).
sandbox:safe_primitive(system:write_term(_,_)).
sandbox:safe_primitive(system:prompt(_,_)).
sandbox:safe_primitive(system:statistics(_,_)).


		 /*******************************
		 *	   REDEFINITION		*
		 *******************************/

pengine_io_goal_expansion(writeln(X),	      pengine_writeln(X)).
pengine_io_goal_expansion(nl,		      pengine_nl).
pengine_io_goal_expansion(format(Fmt),	      pengine_format(Fmt)).
pengine_io_goal_expansion(format(Fmt,Args),   pengine_format(Fmt,Args)).
pengine_io_goal_expansion(read(X),	      pengine_read(X)).
pengine_io_goal_expansion(write_term(X,Opts), pengine_write_term(X,Opts)).
pengine_io_goal_expansion(write(X),	      pengine_write(X)).
pengine_io_goal_expansion(writeq(X),	      pengine_writeq(X)).
pengine_io_goal_expansion(display(X),	      pengine_display(X)).
pengine_io_goal_expansion(print(X),	      pengine_print(X)).
pengine_io_goal_expansion(write_canonical(X), pengine_write_canonical(X)).
pengine_io_goal_expansion(listing,	      pengine_listing).
pengine_io_goal_expansion(listing(X),	      pengine_listing(X)).

