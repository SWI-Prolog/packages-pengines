/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2015, VU University Amsterdam

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
	    pengine_flush_output/0,
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
	    pengine_portray_clause/1,	% +Term

	    pengine_read/1,		% -Term

	    pengine_io_predicate/1,	% ?Head
	    pengine_bind_io_to_html/1,	% +Module
	    pengine_io_goal_expansion/2	% +Goal, -Expanded
	  ]).
:- use_module(library(pengines)).
:- use_module(library(option)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(settings)).
:- use_module(library(error)).
:- use_module(library(listing)).
:- use_module(library(sandbox)).
:- use_module(library(http/html_write)).
:- use_module(library(http/term_html)).
:- if(exists_source(library(prolog_stream))).
:- use_module(library(prolog_stream)).
:- endif.
:- html_meta send_html(html).

:- meta_predicate
	pengine_format(+,:).

/** <module> Provide Prolog I/O for HTML clients

This module redefines some of  the   standard  Prolog  I/O predicates to
behave transparently for HTML clients. It  provides two ways to redefine
the standard predicates: using goal_expansion/2   and  by redefining the
system predicates using redefine_system_predicate/1. The   latter is the
preferred route because it gives a more   predictable  trace to the user
and works regardless of the use of other expansion and meta-calling.

*Redefining* works by redefining the system predicates in the context of
the pengine's module. This  is  configured   using  the  following  code
snippet.

  ==
  :- pengine_application(myapp).
  :- use_module(myapp:library(pengines_io)).
  pengines:prepare_module(Module, myapp, _Options) :-
	pengines_io:pengine_bind_io_to_html(Module).
  ==

*Using goal_expansion/2* works by  rewriting   the  corresponding  goals
using goal_expansion/2 and use the new   definition  to re-route I/O via
pengine_input/2 and pengine_output/1. A pengine  application is prepared
for using this module with the following code:

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
	atomic(Line), \+ special_blob(Line), !,
	send_html(div(class(writeln), Line)).
pengine_writeln(Term) :-
	pengine_module(Module),
	send_html(div(class(writeln),
		      \term(Term,
			    [ quoted(true),
			      module(Module)
			    ]))).

special_blob(X) :-
	blob(X, _Type),
	\+ atom(X).

%%	pengine_nl
%
%	Emit a <br/> to the pengine.

pengine_nl :-
	send_html(br([])).

%%	pengine_flush_output
%
%	No-op.  Pengines do not use output buffering (maybe they should
%	though).

pengine_flush_output.

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
	pengine_write_term(Term, [quoted(true), ignore_ops(true)]).
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
%	in the pengine.

pengine_listing :-
	pengine_listing(_).

pengine_listing(Spec) :-
	pengine_self(Module),
	with_output_to(string(String), listing(Module:Spec)),
	split_string(String, "", "\n", [Pre]),
	send_html(pre(class(listing), Pre)).

pengine_portray_clause(Term) :-
	with_output_to(string(String), portray_clause(Term)),
	split_string(String, "", "\n", [Pre]),
	send_html(pre(class(listing), Pre)).


		 /*******************************
		 *	   PRINT MESSAGE	*
		 *******************************/

:- multifile user:message_hook/3.

%%	user:message_hook(+Term, +Kind, +Lines) is semidet.
%
%	Send output from print_message/2 to   the  pengine. Messages are
%	embedded in a <pre class=msg-Kind></pre> environment.

user:message_hook(Term, Kind, Lines) :-
	Kind \== silent,
	pengine_self(_),
	atom_concat('msg-', Kind, Class),
	phrase(html(pre(class(['prolog-message', Class]),
			\message_lines(Lines))), Tokens),
	with_output_to(string(HTMlString), print_html(Tokens)),
	(   source_location(File, Line)
	->  Src = File:Line
	;   Src = (-)
	),
	pengine_output(message(Term, Kind, HTMlString, Src)).

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

%%	pengines:event_to_json(+Event, -JSON, +Format, +VarNames) is semidet.
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
	pengines:event_to_json/4.

%%	pengines:event_to_json(+PrologEvent, -JSONEvent, +Format, +VarNames)
%
%	If Format equals `'json-s'` or  `'json-html'`, emit a simplified
%	JSON representation of the  data,   suitable  for notably SWISH.
%	This deals with Prolog answers and output messages. If a message
%	originates from print_message/3,  it   gets  several  additional
%	properties:
%
%	  - message:Kind
%	    Indicate the _kind_ of the message (=error=, =warning=,
%	    etc.)
%	  - location:_{file:File, line:Line, ch:CharPos}
%	    If the message is related to a source location, indicate the
%	    file and line and, if available, the character location.

pengines:event_to_json(success(ID, Answers0, Time, More), JSON,
		       'json-s', VarNames) :- !,
	JSON0 = json{event:success, id:ID, time:Time, data:Answers, more:More},
	maplist(answer_to_json_strings(ID), Answers0, Answers),
	add_projection(VarNames, JSON0, JSON).
pengines:event_to_json(output(ID, Term), JSON, 'json-s', _) :- !,
	map_output(ID, Term, JSON).

add_projection(-, JSON, JSON) :- !.
add_projection(VarNames, JSON0, JSON0.put(projection, VarNames)).


%%	answer_to_json_strings(+Pengine, +AnswerDictIn, -AnswerDict).
%
%	Translate answer dict with Prolog term   values into answer dict
%	with string values.

answer_to_json_strings(Pengine, DictIn, DictOut) :-
	dict_pairs(DictIn, Tag, Pairs),
	maplist(term_string_value(Pengine), Pairs, BindingsOut),
	dict_pairs(DictOut, Tag, BindingsOut).

term_string_value(Pengine, N-V, N-A) :-
	with_output_to(string(A),
		       write_term(V,
				  [ module(Pengine),
				    quoted(true)
				  ])).

/* JSON-HTML */

pengines:event_to_json(success(ID, Answers0, Time, More),
		       JSON, 'json-html', VarNames) :- !,
	JSON0 = json{event:success, id:ID, time:Time, data:Answers, more:More},
	maplist(map_answer(ID), Answers0, Answers),
	add_projection(VarNames, JSON0, JSON).
pengines:event_to_json(output(ID, Term), JSON, 'json-html', _) :- !,
	map_output(ID, Term, JSON).

map_answer(ID, Bindings0, Answer) :-
	dict_bindings(Bindings0, Bindings1),
	prolog:translate_bindings(Bindings1, Bindings2, [],
				  ID:Residuals-_HiddenResiduals),
	maplist(binding_to_html(ID), Bindings2, VarBindings),
	(   Residuals == []
	->  Answer = json{variables:VarBindings}
	;   residuals_html(Residuals, ID, ResHTML),
	    Answer = json{variables:VarBindings, residuals:ResHTML}
	).

residuals_html([], _, []).
residuals_html([H0|T0], Module, [H|T]) :-
	term_html_string(H0, [], Module, H),
	residuals_html(T0, Module, T).

dict_bindings(Dict, Bindings) :-
	dict_pairs(Dict, _Tag, Pairs),
	maplist(pair_eq, Pairs, Bindings).

pair_eq(N-V, N=V).

%%	binding_to_html(+Pengine, +Binding, -Dict) is det.
%
%	Convert a variable binding into a JSON Dict. Note that this code
%	assumes that the module associated  with   Pengine  has the same
%	name as the Pengine.  The module is needed to
%
%	@arg Binding is a term binding(Vars,Term,Substitutions)

binding_to_html(ID, binding(Vars,Term,Substitutions), JSON) :-
	JSON0 = json{variables:Vars, value:HTMLString},
	term_html_string(Term, Vars, ID, HTMLString),
	(   Substitutions == []
	->  JSON = JSON0
	;   maplist(subst_to_html(ID), Substitutions, HTMLSubst),
	    JSON = JSON0.put(substitutions, HTMLSubst)
	).

%%	term_html_string(+Term, +VarNames, +Module, -HTMLString) is det.
%
%	Translate  Term  into  an  HTML    string   using  the  operator
%	declarations from Module. VarNames is a   list of variable names
%	that have this value.

term_html_string(Term, Vars, Module, HTMLString) :-
	setting(write_options, Options),
	merge_options(Options,
		      [ quoted(true),
			numbervars(true),
			module(Module)
		      ], WriteOptions),
	phrase(term_html(Term, Vars, WriteOptions), Tokens),
	with_output_to(string(HTMLString), print_html(Tokens)).

%%	binding_term(+Term, +Vars, +WriteOptions)// is semidet.
%
%	Hook to render a Prolog result term as HTML. This hook is called
%	for each non-variable binding,  passing   the  binding  value as
%	Term, the names of the variables as   Vars and a list of options
%	for write_term/3.  If the hook fails, term//2 is called.
%
%	@arg	Vars is a list of variable names or `[]` if Term is a
%		_residual goal_.

:- multifile binding_term//3.

term_html(Term, Vars, WriteOptions) -->
	{ nonvar(Term) },
	binding_term(Term, Vars, WriteOptions), !.
term_html(Term, _Vars, WriteOptions) -->
	term(Term, WriteOptions).

%%	subst_to_html(+Module, +Binding, -JSON) is det.
%
%	Render   a   variable   substitution     resulting   from   term
%	factorization, in this case breaking a cycle.

subst_to_html(ID, '$VAR'(Name)=Value, json{var:Name, value:HTMLString}) :- !,
	term_html_string(Value, [Name], ID, HTMLString).
subst_to_html(_, Term, _) :-
	assertion(Term = '$VAR'(_)).


%%	map_output(+ID, +Term, -JSON) is det.
%
%	Map an output term. This is the same for json-s and json-html.

map_output(ID, message(Term, Kind, HTMLString, Src), JSON) :-
	atomic(HTMLString), !,
	JSON0 = json{event:output, id:ID, message:Kind, data:HTMLString},
	pengines:add_error_details(Term, JSON0, JSON1),
	(   Src = File:Line,
	    \+ JSON1.get(location) = _
	->  JSON = JSON1.put(_{location:_{file:File, line:Line}})
	;   JSON = JSON1
	).
map_output(ID, Term, json{event:output, id:ID, data:Data}) :-
	(   atomic(Term)
	->  Data = Term
	;   term_string(Term, Data)
	).


		 /*******************************
		 *	    SANDBOXING		*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1,	% Goal
	sandbox:safe_meta/2.		% Goal, Called

sandbox:safe_primitive(pengines_io:pengine_listing(_)).
sandbox:safe_primitive(pengines_io:pengine_nl).
sandbox:safe_primitive(pengines_io:pengine_print(_)).
sandbox:safe_primitive(pengines_io:pengine_write(_)).
sandbox:safe_primitive(pengines_io:pengine_write_canonical(_)).
sandbox:safe_primitive(pengines_io:pengine_write_term(_,_)).
sandbox:safe_primitive(pengines_io:pengine_writeln(_)).
sandbox:safe_primitive(pengines_io:pengine_writeq(_)).
sandbox:safe_primitive(pengines_io:pengine_portray_clause(_)).
sandbox:safe_primitive(system:write_term(_,_)).
sandbox:safe_primitive(system:prompt(_,_)).
sandbox:safe_primitive(system:statistics(_,_)).

sandbox:safe_meta(pengines_io:pengine_format(Format, Args), Calls) :-
	sandbox:format_calls(Format, Args, Calls).


		 /*******************************
		 *	   REDEFINITION		*
		 *******************************/

%%	pengine_io_predicate(?Head)
%
%	True when Head describes the  head   of  a (system) IO predicate
%	that is redefined by the HTML binding.

pengine_io_predicate(writeln(_)).
pengine_io_predicate(nl).
pengine_io_predicate(flush_output).
pengine_io_predicate(format(_)).
pengine_io_predicate(format(_,_)).
pengine_io_predicate(read(_)).
pengine_io_predicate(write_term(_,_)).
pengine_io_predicate(write(_)).
pengine_io_predicate(writeq(_)).
pengine_io_predicate(display(_)).
pengine_io_predicate(print(_)).
pengine_io_predicate(write_canonical(_)).
pengine_io_predicate(listing).
pengine_io_predicate(listing(_)).
pengine_io_predicate(portray_clause(_)).

term_expansion(pengine_io_goal_expansion(_,_),
	       Clauses) :-
	findall(Clause, io_mapping(Clause), Clauses).

io_mapping(pengine_io_goal_expansion(Head, Mapped)) :-
	pengine_io_predicate(Head),
	Head =.. [Name|Args],
	atom_concat(pengine_, Name, BodyName),
	Mapped =.. [BodyName|Args].

pengine_io_goal_expansion(_, _).


		 /*******************************
		 *	REBIND PENGINE I/O	*
		 *******************************/

:- if(current_predicate(open_prolog_stream/4)).
:- public
	stream_write/2,
	stream_read/2,
	stream_close/1.

stream_write(_Stream, Out) :-
	send_html(pre(class(console), Out)).
stream_read(_Stream, Data) :-
	prompt(Prompt, Prompt),
	pengine_input(_{type:console, prompt:Prompt}, Data).
stream_close(_Stream).

%%	pengine_bind_user_streams
%
%	Bind the pengine user  I/O  streams   to  a  Prolog  stream that
%	redirects  the  input  and   output    to   pengine_input/2  and
%	pengine_output/1. This results in  less   pretty  behaviour then
%	redefining the I/O predicates to  produce   nice  HTML, but does
%	provide functioning I/O from included libraries.

pengine_bind_user_streams :-
	Err = Out,
	open_prolog_stream(pengines_io, write, Out, []),
	set_stream(Out, buffer(line)),
	open_prolog_stream(pengines_io, read,  In, []),
	set_stream(In,  alias(user_input)),
        set_stream(Out, alias(user_output)),
        set_stream(Err, alias(user_error)),
	set_stream(In,  alias(current_input)),
        set_stream(Out, alias(current_output)),
	thread_at_exit(close_io(In, Out)).

close_io(In, Out) :-
	close(In, [force(true)]),
	close(Out, [force(true)]).
:- else.

pengine_bind_user_streams.

:- endif.


%%	pengine_bind_io_to_html(+Module)
%
%	Redefine the built-in predicates for IO   to  send HTML messages
%	using pengine_output/1.

pengine_bind_io_to_html(Module) :-
	forall(pengine_io_predicate(Head),
	       bind_io(Head, Module)),
	pengine_bind_user_streams.

bind_io(Head, Module) :-
	prompt(_, ''),
	redefine_system_predicate(Module:Head),
	functor(Head, Name, Arity),
	Head =.. [Name|Args],
	atom_concat(pengine_, Name, BodyName),
	Body =.. [BodyName|Args],
	assertz(Module:(Head :- Body)),
	compile_predicates([Module:Name/Arity]).
