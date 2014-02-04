/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, Torbjörn Lager,
			 VU University Amsterdam

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

:- module(pengine,
	  [ pengine_create/1,			% +Options
            pengine_send/2,
            pengine_send/3,
            pengine_ask/2,
            pengine_ask/3,
            pengine_next/1,
            pengine_next/2,
            pengine_stop/1,
            pengine_stop/2,
            pengine_event/1,
            pengine_event/2,
            pengine_input/1,
            pengine_set_prompt/1,
            pengine_get_prompt/1,
            pengine_output/1,
            pengine_output/2,
            pengine_pull_response/1,
            pengine_pull_response/2,
            pengine_destroy/1,
            pengine_abort/1,
            pengine_property/2,
            pengine_event_loop/1,
            pengine_event_loop/2,
            pengine_rpc/2,
            pengine_rpc/3,
            pengine_ask_around/2,
            pengine_ask_around/3,
            pengine_seek_agreement/2,
            pengine_seek_agreement/3
	  ]).

/** <module> Pengines: Web Logic Programming Made Easy

The library(pengines) provides an  infrastructure   for  creating Prolog
engines in a (remote) pengine server  and accessing these engines either
from Prolog or JavaScript.

@author Torbjörn Lager and Jan Wielemaker
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(uri)).
:- use_module(library(filesex)).
:- use_module(library(time)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(apply)).
:- use_module(library(aggregate)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(sandbox)).
:- use_module(library(term_to_json)).
:- if(exists_source(library(uuid))).
:- use_module(library(uuid)).
:- endif.


:- meta_predicate
	pengine_create(:),
	pengine_rpc(+, +, :),
	pengine_event_loop(1),
	pengine_event_loop(1, +),
	pengine_ask_around(+, 0),
	pengine_ask_around(+, 0, +),
	pengine_seek_agreement(+, 0),
	pengine_seek_agreement(+, 0, +).

:- predicate_options(pengine_create/1, 1,
		     [ id(-atom),
		       name(atom),
		       server(atom),
		       src_list(list),
		       src_text(any),		% text
		       src_url(atom),
		       src_predicates(list),
		       probe(callable),
		       probe_template(any),
		       format(oneof([prolog,json,'json-s']))
		     ]).
:- predicate_options(pengine_ask/3, 3,
		     [ template(any),
		       paging(integer)
		     ]).
:- predicate_options(pengine_next/2, 2,
		     [ pass_to(pengine_send/3, 3)
		     ]).
:- predicate_options(pengine_stop/2, 2,
		     [ pass_to(pengine_send/3, 3)
		     ]).
:- predicate_options(pengine_output/2, 2,
		     [ pass_to(pengine_send/3, 3)
		     ]).
:- predicate_options(pengine_rpc/3, 3,
		     [ paging(integer),
		       pass_to(pengine_create/1, 1)
		     ]).
:- predicate_options(pengine_send/3, 3,
		     [ delay(number)
		     ]).
:- predicate_options(pengine_event/2, 2,
		     [ pass_to(thread_get_message/3, 3)
		     ]).
:- predicate_options(pengine_pull_response/2, 2,
		     [ pass_to(http_open/3, 3)
		     ]).
:- predicate_options(pengine_event_loop/2, 2,
		     []).			% not yet implemented
:- predicate_options(pengine_ask_around/3, 3,
		     [ use_local(boolean),
		       pass_to(pengine_rpc/3, 3)
		     ]).
:- predicate_options(pengine_seek_agreement/3, 3,
		     [ use_local(boolean),
		       pass_to(pengine_rpc/3, 3)
		     ]).

% :- debug(pengine(transition)).



:- multifile
	sandbox:safe_primitive/1,		% Goal
	sandbox:safe_meta/2.			% Goal, Calls

sandbox:safe_primitive(pengine:pengine_create(_)).
sandbox:safe_primitive(pengine:pengine_event(_)).
sandbox:safe_primitive(pengine:pengine_event(_, _)).
sandbox:safe_primitive(pengine:pengine_send(_, _, _)).
sandbox:safe_primitive(pengine:pengine_input(_)).
sandbox:safe_primitive(pengine:pengine_output(_, _)).
sandbox:safe_primitive(pengine:pengine_rpc(_, _, _)).
sandbox:safe_primitive(pengine:pengine_event_loop(_)).

sandbox:safe_primitive(system:sleep(_)).
sandbox:safe_primitive(system:atom_concat(_, _, _)).


/* Settings */

:- setting(max_session_pengines, integer, 1,
	   'Maximum number of pengines a client can create.  -1 is infinite.').

:- setting(time_limit, number, 60, 'Maximum time between output').

:- setting(pengine_alive_time_limit, number, 60,
	   'Maximum time to allow a pengine to live').

:- setting(max_pengines, integer, 200,
	   'Maximum number of pengines that can be alive').

:- setting(allow_from, list(atom), [*],
	   'IP addresses from which remotes are allowed to connect').
:- setting(deny_from, list(atom), [],
	   'IP addresses from which remotes are NOT allowed to connect').

:- meta_predicate			% internal meta predicates
	solve(?, 0, +),
	pengine_event_loop(1, +, +),
	pengine_event_loop(+, 1, +, +),
	pengine_find_n(+, ?, 0, -).

/**  pengine_create(:Options) is det.

    Creates a new pengine. Valid options are:

    * id(-ID)
      ID gets instantiated to the id of the pengine. The id is a complex
      term, its structure will remain undocumented and should not be
      relied on.

    * name(+Name)
      The pengine is named Name (an atom). A slave pengine (child) can
      subsequently be referred to by this name, but only by its master
      (parent). The atoms =parent= and =self= are reserved names and
      must not be used here.

    * server(+URL)
      The pengine will run in (and in the Prolog context of) the pengine
      server located at URL.

    * src_list(+List_of_clauses)
      Inject a list of Prolog clauses in the pengine.

    * src_text(+Atom_or_string)
      Inject the clauses specified by a source text in the pengine.

    * src_url(+URL)
      Inject the clauses specified in the file located at URL in the
      pengine.

    * src_predicates(+List)
      Send the local predicates denoted by List to the remote pengine.
      List is a list of predicate indicators.

    * probe(+Query)
      Run Query before creating the pengine. If the query fails, the
      pengine is not created. Makes sense only if the pengine is to be
      run remotely. Query is `true' by default.

    * probe_template(+Template)
      Template is a term possibly containing variables shared with the
      probe query. By default, the template is identical to the probe
      query. The second argument of the =create= event will be bound to
      an instance of this term, which makes it useful for getting
      information about the environment in which the pengine is to be
      run.

    * format(+Format)
      Determines the format of event responses. Format is an atom,
      either =prolog= (default), =json=, or =json-s=.

Remaining  options  are  passed  to  http_open/3  (meaningful  only  for
non-local pengines) and thread_create/3. Note   that for thread_create/3
only options changing the stack-sizes can be used. In particular, do not
pass the detached or alias options..

Successful creation of a pengine will return an _event term_ of the
following form:

    * create(ID, Term)
      ID is the id of the pengine that was created.
      Term is an instance of Template.

An error will be returned if the pengine could not be created:

    * error(ID, Term)
      ID is invalid, since no pengine was created.
      Term is the exception's error term.
*/


pengine_create(QOptions) :-
    meta_options(is_meta, QOptions, Options),
    (   select_option(server(BaseURL), Options, RestOptions)
    ->  remote_pengine_create(BaseURL, RestOptions)
    ;   local_pengine_create(Options)
    ).

is_meta(src_predicates).

/**  pengine_send(+NameOrID, +Term) is det

Same as pengine_send(NameOrID, Term, []).
*/

pengine_send(Target, Event) :-
    pengine_send(Target, Event, []).


/**  pengine_send(+NameOrID, +Term, +Options) is det

Succeeds immediately and  places  Term  in   the  queue  of  the pengine
NameOrID. Options is a list of options:

   * delay(+Time)
     The actual sending is delayed by Time seconds. Time is an integer
     or a float.

Any remaining options are passed to http_open/3.
*/

pengine_send(Target, Event, Options) :-
    must_be(atom, Target),
    pengine_send2(Target, Event, Options).

pengine_send2(parent, Event0, Options) :- !,
    pengine_parent(Queue),
    (   Event0 = output(_, _)
    ->  Event = Event0
    ;   pengine_self(Self),
	Event = output(Self, Event0)
    ),
    delay_message(queue(Queue), Event, Options).
pengine_send2(self, Event, Options) :- !,
    thread_self(Queue),
    delay_message(queue(Queue), Event, Options).
pengine_send2(Name, Event, Options) :-
    named_child(Name, Target), !,
    delay_message(pengine(Target), Event, Options).
pengine_send2(Target, Event, Options) :-
    delay_message(pengine(Target), Event, Options).

delay_message(Target, Event, Options) :-
    option(delay(Delay), Options), !,
    alarm(Delay,
	  send_message(Target, Event, Options),
	  _AlarmID,
	  [remove(true)]).
delay_message(Target, Event, Options) :-
    send_message(Target, Event, Options).

send_message(queue(Queue), Event, _) :-
    thread_send_message(Queue, Event).
send_message(pengine(Pengine), Event, Options) :-
    (	pengine_remote(Pengine, Server)
    ->	remote_pengine_send(Server, Pengine, Event, Options)
    ;	pengine_thread(Pengine, Thread),
	thread_send_message(Thread, Event)
    ).

%%	pengine_reply(+Event) is det.
%%	pengine_reply(+Queue, +Event) is det.
%
%	Reply Event to the parent of the   current  Pengine or the given
%	Queue.

pengine_reply(Event) :-
    nb_getval(pengine_parent, Queue),
    pengine_reply(Queue, Event).

pengine_reply(Queue, Event) :-
    debug(pengine(event), 'Reply to ~p: ~p', [Queue, Event]),
    thread_send_message(Queue, Event).


/** pengine_ask(+NameOrID, @Query) is det

Same as pengine_ask(NameOrID, Query, []).

*/

pengine_ask(ID, Query) :-
    pengine_ask(ID, Query, []).

/** pengine_ask(+NameOrID, @Query, +Options) is det

Asks pengine NameOrID a query Query.

Options is a list of options:

    * template(+Template)
      Template is a variable (or a term containing variables) shared
      with the query. By default, the template is identical to the
      query.

    * paging(+Integer)
      Retrieve solutions in chunks of Integer rather than one by one. 0
      means no paging (default). Other integers indicate the maximum
      number of solutions to retrieve in one chunk.

Any remaining options are passed to pengine_send/3.

Note that the predicate pengine_ask/3 is deterministic, even for queries
that have more than one solution. Also,  the variables in Query will not
be bound. Instead, results will  be  returned   in  the  form  of _event
terms_.

    * success(ID, Terms, More)
      ID is the id of the pengine that succeeded in solving the query.
      Terms is a list holding instantiations of `Template`. More is
      either `true` or `false`, indicating whether we can expect the
      pengine to be able to return more solutions or not, would we call
      pengine_next/1.

    * failure(ID)
      ID is the id of the pengine that failed for lack of a solutions.

    * error(ID, Term)
      ID is the id of the pengine throwing the exception.
      Term is the exception's error term.

    * output(ID, Term)
      ID is the id of a pengine running the query that called
      pengine_output/1. Term is the term that was passed in the first
      argument of pengine_output/1 when it was called.

    * prompt(ID, Term)
      ID is the id of the pengine that called pengine_input/1.
      Term is the current prompt, as set by pengine_set_prompt/1.

Defined in terms of pengine_send/3, like so:

==
pengine_ask(ID, Query, Options) :-
    partition(pengine_ask_option, Options, AskOptions, SendOptions),
    pengine_send(ID, request(ask(Query, AskOptions)), SendOptions).
==
*/

pengine_ask(ID, Query, Options) :-
    partition(pengine_ask_option, Options, AskOptions, SendOptions),
    pengine_send(ID, request(ask(Query, AskOptions)), SendOptions).


pengine_ask_option(template(_)).
pengine_ask_option(paging(_)).


/** pengine_next(+NameOrID) is det

Same as pengine_next(NameOrID, []).
*/

pengine_next(ID) :- pengine_send(ID, request(next)).


/** pengine_next(+NameOrID, +Options) is det

Asks pengine NameOrID for the next solution to a query started by
pengine_ask/3. Options are passed to pengine_send/3.

Here too, results will be returned in the form of _event terms_.

    * success(ID, Terms, More)
      ID is the id of the pengine that succeeded in finding yet another
      solution to the query. Terms is a list holding instantiations of
      `Template`. More is either `true` or `false`, indicating whether
      we can expect the pengine to be able to return more solutions or
      not, would we call pengine_next/1.

    * failure(ID)
      ID is the id of the pengine that failed for lack of more solutions.

    * error(ID, Term)
      ID is the id of the pengine throwing the exception.
      Term is the exception's error term.

    * output(ID, Term)
      ID is the id of a pengine running the query that called
      pengine_output/1. Term is the term that was passed in the first
      argument of pengine_output/1 when it was called.

    * prompt(ID, Term)
      ID is the id of the pengine that called pengine_input/1.
      Term is the current prompt, as set by pengine_set_prompt/1.

Defined in terms of pengine_send/3, as follows:

==
pengine_next(ID, Options) :-
    pengine_send(ID, request(next), Options).
==

*/

pengine_next(ID, Options) :- pengine_send(ID, request(next), Options).


/** pengine_stop(+NameOrID) is det

Same as pengine_stop(NameOrID, []).

*/

pengine_stop(ID) :- pengine_send(ID, request(stop)).

/** pengine_stop(+NameOrID, +Options) is det

Tells pengine NameOrID to stop looking  for   more  solutions to a query
started by pengine_ask/3. Options are passed to pengine_send/3.

Defined in terms of pengine_send/3, like so:

==
pengine_stop(ID, Options) :-
    pengine_send(ID, request(stop), Options).
==
*/

pengine_stop(ID, Options) :- pengine_send(ID, request(stop), Options).


/** pengine_abort(+NameOrID) is det

Aborts the running query. The pengine goes   back  to state `2', waiting
for new queries.

@see pengine_destroy/1.
*/

pengine_abort(Pengine) :- !,
    pengine_remote(Pengine, Server), !,
    remote_pengine_abort(Server, Pengine, []).
pengine_abort(Pengine) :-
    pengine_thread(Pengine, Thread),
    catch(thread_signal(Thread, throw(abort_query)), _, true).


/** pengine_destroy(+NameOrID) is det

Destroys the pengine NameOrID.

@tbd	Should abort the pengine if it is running a query.
*/

pengine_destroy(ID) :-
    pengine_send(ID, request(destroy)).


/*================= pengines administration =======================
*/

%%	current_pengine(?Id, ?Parent, ?Location)
%
%	Dynamic predicate that registers our known pengines.  Id is
%	an atomic unique datatype.  Parent is the id of our parent
%	pengine.  Location is one of
%
%	  - thread(ThreadId)
%	  - remote(URL)

:- dynamic
	current_pengine/4.		% Id, ParentId, Thread, URL
:- volatile
	current_pengine/4.

:- thread_local
	child/1,			% ?Child
	named_child/2.			% ?Name, ?Child

%%	pengine_register_local(-Id, +Thread, +Queue, +URL) is det.
%%	pengine_register_remote(+Id, +URL, +Queue) is det.
%%	pengine_unregister(+Id) is det.

pengine_register_local(Id, Thread, Queue, URL) :-
    uuid(Id),
    asserta(current_pengine(Id, Queue, Thread, URL)).

pengine_register_remote(Id, URL) :-
    thread_self(Queue),
    asserta(current_pengine(Id, Queue, 0, URL)).

pengine_unregister(Id) :-
    retractall(current_pengine(Id, _, _, _)).

pengine_self(Id) :-
    thread_self(Thread),
    current_pengine(Id, _Parent, Thread, _URL).

pengine_parent(Parent) :-
    nb_getval(pengine_parent, Parent).

http_pengine_parent(Pengine, Parent) :-
    current_pengine(Pengine, Parent, Thread, _URL),
    Thread \== 0, !.

pengine_thread(Pengine, Thread) :-
    current_pengine(Pengine, _Parent, Thread, _URL),
    Thread \== 0, !.

pengine_remote(Pengine, URL) :-
    current_pengine(Pengine, _Parent, 0, URL).

:- if(\+current_predicate(uuid/1)).
:- use_module(library(random)).
uuid(Id) :-
    Max is 1<<128,
    random_between(0, Max, Num),
    atom_number(Id, Num).
:- endif.


/** pengine_property(+NameOrID, ?Property) is nondet.

True  when  Property  is  a  property  of  the  given  Pengine.  Defined
properties are:

  * parent(Thread)
    Thread id for the parent (local) pengine.
  * self(Thread)
    Thread id of the running pengine.
*/


pengine_property(Id, parent(Parent)) :-
    current_pengine(Id, Parent, _Thread, _URL).
pengine_property(Id, self(Id)) :-
    current_pengine(Id, _Parent, _Thread, _URL).
pengine_property(Id, remote(Server)) :-
    current_pengine(Id, _Parent, 0, Server).


/** pengine_output(+Term) is det

Same as pengine_output(Term, []).

*/

pengine_output(Term) :- pengine_output(Term, []).


/** pengine_output(+Term, Options) is det

Sends Term to the  parent  pengine  or   thread.  Defined  in  terms  of
pengine_send/3, like so:

==
pengine_output(Term, Options) :-
    pengine_send(parent, Term, Options).
==
*/

pengine_output(Term, Options) :- pengine_send(parent, Term, Options).



/*================= Local pengine =======================
*/

%%	local_pengine_create(+Options)
%
%	Creates  a  local   Pengine,   which    is   a   thread  running
%	pengine_main/2.  It maintains two predicates:
%
%	  - The global dynamic predicate id/2 relates Pengines to their
%	    childs.
%	  - The local predicate id/2 maps named childs to their ids.

local_pengine_create(Options) :-
    thread_self(Self),
    create(Self, Child, Options, local),
    assert(child(Child)),
    (   option(name(Name), Options)
    ->  assert(named_child(Name, Child))
    ;   true
    ).

%%	create(+Queue, -Child, +Options, +URL) is det.
%
%	Create a new pengine thread.
%
%	@arg Queue is the queue (or thread handle) to report to
%	@arg Child is the identifier of the created pengine.

create(Queue, Child, Options, URL) :-
    select_option(probe(Condition), Options, RestOptions0, true),
    partition(pengine_create_option, RestOptions0, PengineOptions, RestOptions),
    (   catch(Condition, E, true)
    ->  (   var(E)
	->  thread_create(
		pengine_main(Queue, PengineOptions), ChildThread,
		[ at_exit(pengine_done)
		| RestOptions
		]),
	    pengine_register_local(Child, ChildThread, Queue, URL),
	    thread_send_message(ChildThread, pengine_registered(Child)),
	    (	option(id(Id), Options)
	    ->	Id = Child
	    ;	true
	    )
	;   probe_failure(Queue, E)
	)
    ;   probe_failure(Queue, error(probe_failure(Condition), _))
    ).

probe_failure(Queue, Term) :-
    pengine_reply(Queue, error(id(null, null), Term)).


pengine_create_option(src_text(_)).
pengine_create_option(src_list(_)).
pengine_create_option(src_url(_)).
pengine_create_option(src_predicates(_)).
pengine_create_option(probe(_)).
pengine_create_option(probe_template(_)).

%%	pengine_done is det.
%
%	Called  from  the  pengine  thread  =at_exit=  option.  Destroys
%	_child_ pengines using pengine_destroy/1.

:- public
	pengine_done/0.

pengine_done :-
    forall(retract(child(Child)),
	   pengine_destroy(Child)),
    pengine_self(Id),
    pengine_unregister(Id).


%%	pengine_main(+Parent, +Options)
%
%	Run a pengine main loop. First acknowledges its creation and run
%	pengine_main_loop/1.

pengine_main(Parent, Options) :-
    fix_streams,
    thread_get_message(pengine_registered(Self)),
    nb_setval(pengine_parent, Parent),
    select_option(probe_template(Template), Options, RestOptions, true),
    (   catch(maplist(process_create_option, RestOptions), Error,
	      ( send_error(Error),
		fail
	      ))
    ->  pengine_reply(create(Self, Template)),
        pengine_main_loop(Self)
    ;   true
    ).

%%	fix_streams is det.
%
%	If we are a pengine that is   created  from a web server thread,
%	the current output points to a CGI stream.

fix_streams :-
	fix_stream(current_output).

fix_stream(Name) :-
	is_cgi_stream(Name), !,
	debug(pengine(stream), '~w is a CGI stream!', [Name]),
	set_stream(user_output, alias(Name)).
fix_stream(_).


process_create_option(src_list(ClauseList)) :- !,
	pengine_src_list(ClauseList).
process_create_option(src_text(Text)) :- !,
	pengine_src_text(Text).
process_create_option(src_url(URL)) :- !,
	pengine_src_url(URL).
process_create_option(_).


pengine_main_loop(ID) :-
    catch(guarded_main_loop(ID), abort_query,
	  ( pengine_reply(abort(ID)),
	    pengine_main_loop(ID)
	  )).


%%	guarded_main_loop(+Pengine) is det.
%
%	Executes state `2' of  the  pengine,   where  it  waits  for two
%	events:
%
%	  - destroy
%	  Terminate the pengine
%	  - ask(:Goal, +Options)
%	  Solve Goal.

guarded_main_loop(ID) :-
    pengine_event(Event),
    (   Event = request(destroy)
    ->  debug(pengine(transition), '~q: 2 = ~q => 1', [ID, destroy]),
	pengine_terminate(ID)
    ;   Event = request(ask(Goal, Options))
    ->  debug(pengine(transition), '~q: 2 = ~q => 3', [ID, ask(Goal)]),
        ask(ID, Goal, Options)
    ;   debug(pengine(transition), '~q: 2 = ~q => 2', [ID, protocol_error]),
        %pengine_reply(error(ID, error(protocol_error, _))),
        guarded_main_loop(ID)
    ).


pengine_terminate(ID) :-
    pengine_reply(destroy(ID)),
    thread_self(Me),		% Make the thread silently disappear
    thread_detach(Me).


%%	solve(+Template, :Goal, +ID) is det.
%
%	Solve Goal. Note that because we can ask for a new goal in state
%	`6', we must provide for an ancesteral cut (prolog_cut_to/1). We
%	need to be sure to  have  a   choice  point  before  we can call
%	prolog_current_choice/1. This is the reason   why this predicate
%	has two clauses.

solve(Template, Goal, ID) :-
    prolog_current_choice(Choice),
    (   call_cleanup(catch(Goal, Error, true), Det=true),
        (   var(Error)
        ->  (   var(Det)
            ->  pengine_reply(success(ID, Template, true)),
                more_solutions(ID, Choice)
            ;   !,			% commit
		pengine_reply(success(ID, Template, false)),
                guarded_main_loop(ID)
            )
        ;   !,				% commit
	    pengine_reply(error(ID, Error)),
            guarded_main_loop(ID)
        )
    ;   !,				% commit
	pengine_reply(failure(ID)),
        guarded_main_loop(ID)
    ).
solve(_, _, _).				% leave a choice point

%%	more_solutions(+Pengine, +Choice)
%
%	Called after a solution was found while  there can be more. This
%	is state `6' of the state machine. It processes these events:
%
%	  * stop
%	  Go back via state `7' to state `2' (guarded_main_loop/1)
%	  * next
%	  Fail.  This causes solve/3 to backtrack on the goal asked.
%	  * ask(Goal, Options)
%	  Ask another goal.  Note that we must commit the choice point
%	  of the previous goal asked for.

more_solutions(ID, Choice) :-
    pengine_event(request(Event)),
    more_solutions(Event, ID, Choice).

more_solutions(stop, ID, _Choice) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 7', [ID, stop]),
    pengine_reply(stop(ID)),
    guarded_main_loop(ID).
more_solutions(next, ID, _Choice) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, next]),
    fail.
more_solutions(ask(Goal, Options), ID, Choice) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, ask(Goal)]),
    prolog_cut_to(Choice),
    ask(ID, Goal, Options).
more_solutions(destroy, ID, _Choice) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 1', [ID, destroy]),
    pengine_terminate(ID).
more_solutions(Event, ID, Choice) :-
    debug(pengine(transition), '~q: 6 = ~q => 6', [ID, protocol_error(Event)]),
    pengine_reply(error(ID, error(protocol_error, _))),
    more_solutions(ID, Choice).

%%	ask(+Pengine, :Goal, +Options)
%
%	Migrate from state `2' to `3'.  This predicate validates that it
%	is safe to call Goal using safe_goal/1 and then calls solve/3 to
%	prove the goal. It takes care of the paging(N) option.
%
%	@tbd Assumes goal is called in the module =pengine=; this will
%	be changed.

ask(ID, Goal, Options) :-
    expand_goal(pengine:Goal, Goal1),
    catch(safe_goal(Goal1), Error, true),
    (   var(Error)
    ->  option(template(Template), Options, Goal),
        option(paging(N), Options, 1),
        (   N == 1
        ->  solve([Template], Goal1, ID)
        ;   solve(Res, pengine_find_n(N, Template, Goal1, Res), ID)
        )
    ;   pengine_reply(error(ID, Error))
    ).



/** pengine_pull_response(+NameOrID) is det

Same as pengine_pull_response(ID, []).

*/

pengine_pull_response(ID) :-
    pengine_pull_response(ID, []).


/** pengine_pull_response(+NameOrID, +Options) is det

Pulls a response (an event term)  from   the  slave  process NameOrID if
NameOrID is a remote process, else does nothing at all.

*/

pengine_pull_response(BaseURL:ID, Options) :- !,
    remote_pengine_pull_response(BaseURL, BaseURL:ID, Options).
pengine_pull_response(_ID, _Options).



/** pengine_input(-Term) is det

Sends a prompt (as set by   pengine_set_prompt/1)  to the parent pengine
and waits for input.

*/

pengine_input(Term) :-
    pengine_self(Self),
    nb_getval(pengine_parent, Parent),
    pengine_get_prompt(Prompt),
    pengine_reply(Parent, prompt(Self, Prompt)),
    pengine_event(input(Term)).



/** pengine_set_prompt(+Term) is det

Sets the prompt associated with pengine_input/1.   Note that Term may be
any complex term.

*/

:- thread_local pengine_current_prompt/1.

pengine_set_prompt(Prompt) :-
    retractall(pengine_current_prompt(_)),
    assert(pengine_current_prompt(Prompt)).


/** pengine_get_prompt(-Term) is det

Gets the current pengine prompt.

*/

pengine_get_prompt(Prompt) :-
    (   pengine_current_prompt(Prompt)
    ->  true
    ;   Prompt = '|:'
    ).


%%	send_error(+Error) is det.
%
%	Send an error to my parent.   Remove non-readable blobs from the
%	error term first using replace_blobs/2.

send_error(Error) :-
    pengine_self(Self),
    replace_blobs(Error, Error1),
    pengine_reply(error(Self, Error1)).

%%	replace_blobs(Term0, Term) is det.
%
%	Copy Term0 to Term, replacing non-text   blobs. This is required
%	for error messages that may hold   streams  and other handles to
%	non-readable objects.

replace_blobs(Blob, Atom) :-
    blob(Blob, Type), Type \== text, !,
    format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term) :-
    compound(Term0), !,
    compound_name_arguments(Term0, Name, Args0),
    maplist(replace_blobs, Args0, Args),
    compound_name_arguments(Term, Name, Args).
replace_blobs(Term, Term).


/*================= Remote pengines =======================
*/


remote_pengine_create(BaseURL, Options) :-
    partition(pengine_create_option, Options, PengineOptions0, RestOptions),
    translate_local_source(PengineOptions0, PengineOptions),
    options_to_dict(PengineOptions, PostData),
    remote_post_rec(BaseURL, create, PostData, Reply, RestOptions),
    arg(1, Reply, ID),
    (	option(id(ID2), Options)
    ->	ID = ID2
    ;	true
    ),
    (   option(name(Name), Options)
    ->  assert(named_child(Name, ID))
    ;   true
    ),
    pengine_register_remote(ID, BaseURL),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

translate_local_source(PengineOptions0, PengineOptions) :-
    select_option(src_predicates(M:List), PengineOptions0, PengineOptions1), !,
    with_output_to(string(Text), M:maplist(listing, List)),
    PengineOptions = [src_text(Text)|PengineOptions1].
translate_local_source(Options, Options).

options_to_dict(Options, Dict) :-
    select_option(probe_template(Template), Options, Options1),
    select_option(probe(Probe), Options1, Options2), !,
    numbervars(Probe+Template, 0, _),
    format(string(ProbeString), '~k', [Probe]),
    format(string(TemplateString), '~k', [Template]),
    maplist(prolog_option, Options2, Options3),
    dict_create(Dict, _,
		[ probe(ProbeString),
		  probe_template(TemplateString)
		| Options3
		]).
options_to_dict(Options, Dict) :-
    maplist(prolog_option, Options, Options1),
    dict_create(Dict, _, Options1).

prolog_option(Option0, Option) :-
    prolog_option(Option0), !,
    Option0 =.. [Name,Value],
    format(string(String), '~k', [Value]),
    Option =.. [Name,String].
prolog_option(Option, Option).

prolog_option(src_list(_)).


remote_pengine_send(BaseURL, ID, Event, Options) :-
    term_to_atom(Event, EventAtom),
    remote_send_rec(BaseURL, send, [id=ID, event=EventAtom], Reply, Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

remote_pengine_pull_response(BaseURL, ID, Options) :-
    remote_send_rec(BaseURL, pull_response, [id=ID], Reply, Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

remote_pengine_abort(BaseURL, ID, Options) :-
    remote_send_rec(BaseURL, abort, [id=ID], Reply, Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

%%	remote_send_rec(+Server, +Action, +Params, -Reply, +Options)
%
%	Issue a GET request on Server and   unify Reply with the replied
%	term.

remote_send_rec(Server, Action, Params, Reply, Options) :-
    server_url(Server, Action, Params, URL),
    http_open(URL, Stream, Options),	% putting this in setup_call_cleanup/3
    call_cleanup(			% makes it impossible to interrupt.
	read(Stream, Reply),
	close(Stream)).

remote_post_rec(Server, Action, Data, Reply, Options) :-
    server_url(Server, Action, [], URL),
    http_open(URL, Stream,
	      [ post(json(Data))
	      | Options
	      ]),
    call_cleanup(			% makes it impossible to interrupt.
	read(Stream, Reply),
	close(Stream)).

server_url(Server, Action, Params, URL) :-
    uri_components(Server, Components0),
    uri_query_components(Query, Params),
    uri_data(path, Components0, Path0),
    atom_concat('pengine/', Action, PAction),
    directory_file_path(Path0, PAction, Path),
    uri_data(path, Components0, Path, Components),
    uri_data(search, Components, Query),
    uri_components(URL, Components).


/** pengine_event(?EventTerm) is det.
    pengine_event(?EventTerm, +Options) is det.

Examines the pengine's event queue  and   if  necessary blocks execution
until a term that unifies to Term  arrives   in  the queue. After a term
from the queue has been unified to Term,   the  term is deleted from the
queue.

   Valid options are:

   * timeout(+Time)
     Time is a float or integer and specifies the maximum time to wait
     in seconds. If no event has arrived before the time is up EventTerm
     is bound to the atom =timeout=.
*/

pengine_event(Event) :-
    pengine_event(Event, []).

pengine_event(Event, Options) :-
    thread_self(Self),
    (   thread_get_message(Self, Event, Options)
    ->  true
    ;   Event = timeout
    ),
    update_remote_destroy(Event).

update_remote_destroy(destroy(Id)) :-
    pengine_remote(Id, _Server), !,
    pengine_unregister(Id).
update_remote_destroy(_).


/** pengine_event_loop(:Closure) is det.
    pengine_event_loop(:Closure, +Options) is det

Starts an event loop accepting event terms   sent to the current pengine
or thread. For each such  event   E,  calls  ignore(call(Closure, E)). A
closure thus acts as a _handler_  for   the  event. Some events are also
treated specially:

   * create(ID, Term)
     The ID is placed in a list of active pengines.

   * destroy(ID)
     The ID is removed from the list of active pengines. When the last
     pengine ID is removed, the loop terminates.

   * output(ID, Term)
     The predicate pengine_pull_response/2 is called.

Valid options are:

   * autoforward(+To)
     Forwards received event terms to slaves. To is either =all=,
     =all_but_sender= or a Prolog list of NameOrIDs. [not yet
     implemented]

*/

pengine_event_loop(Closure) :-
    pengine_event_loop(Closure, [], []).

pengine_event_loop(Closure, Options) :-
    pengine_event_loop(Closure, [], Options).

pengine_event_loop(Closure, Created, Options) :-
    pengine_event(Event),
    (   option(autoforward(all), Options) % TODO: Implement all_but_sender and list of IDs
    ->  forall(member(ID, Created), pengine_send(ID, Event))
    ;   true
    ),
    pengine_event_loop(Event, Closure, Created, Options).

pengine_event_loop(create(ID, T), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 1 = /~q => 2', [ID, create(T)]),
    ignore(call(Closure, create(ID, T))),
    pengine_event_loop(Closure, [ID|Created], Options).
pengine_event_loop(output(ID, Msg), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 4', [ID, output(Msg)]),
    ignore(call(Closure, output(ID, Msg))),
    pengine_pull_response(ID, []),
    pengine_event_loop(Closure, Created, Options).
pengine_event_loop(prompt(ID, Term), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 5', [ID, prompt(Term)]),
    ignore(call(Closure, prompt(ID, Term))),
    pengine_event_loop(Closure, Created, Options).
pengine_event_loop(success(ID, Sol, More), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 6/2', [ID, success(Sol, More)]),
    ignore(call(Closure, success(ID, Sol, More))),
    pengine_event_loop(Closure, Created, Options).
pengine_event_loop(failure(ID), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 2', [ID, failure]),
    ignore(call(Closure, failure(ID))),
    pengine_event_loop(Closure, Created, Options).
pengine_event_loop(error(ID, Msg), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 2', [ID, error(Msg)]),
    ignore(call(Closure, error(ID, Msg))),
    pengine_event_loop(Closure, Created, Options).
pengine_event_loop(stop(ID), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 7 = /~q => 2', [ID, stop]),
    ignore(call(Closure, stop(ID))),
    pengine_event_loop(Closure, Created, Options).
pengine_event_loop(destroy(ID), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 1 = /~q => 0', [ID, destroy]),
    ignore(call(Closure, destroy(ID))),
    delete(Created, ID, RestCreated),
    (   RestCreated == []
    ->  debug(pengine(event), '*** Event loop terminated', []),
        true
    ;   pengine_event_loop(Closure, RestCreated, Options)
    ).


/** pengine_rpc(+URL, +Query) is nondet.
    pengine_rpc(+URL, +Query, +Options) is nondet.

Semantically equivalent to the sequence below,  except that the query is
executed in (and in the Prolog context   of) the pengine server referred
to by URL, rather than locally.

  ==
    copy_term(Query, Copy),
    call(Copy),			% executed on server at URL
    Query = Copy.
  ==

Valid options are:

    * paging(+Integer)
      Can be used to reduce the number of network roundtrips being made.
      See pengine_ask/3.

Remaining  options  (except   the   server    option)   are   passed  to
pengine_create/1.
*/

pengine_rpc(URL, Query) :-
    pengine_rpc(URL, Query, []).

pengine_rpc(URL, Query, QOptions) :-
    meta_options(is_meta, QOptions, Options),
    term_variables(Query, Vars),
    Template =.. [v|Vars],
    setup_call_cleanup(
	pengine_create([ server(URL),
			 id(Id)
		       | Options
		       ]),
	wait_event(Query, Template, Options),
	pengine_destroy_and_wait(Id)).

pengine_destroy_and_wait(Id) :-
	pengine_destroy(Id),
	pengine_event(destroy(Id)).

wait_event(Query, Template, Options) :-
    pengine_event(Event),
    process_event(Event, Query, Template, Options).

process_event(create(ID, _), Query, Template, Options) :-
    pengine_ask(ID, Query, [template(Template)|Options]),
    wait_event(Query, Template, Options).
process_event(error(_ID, Error), _Query, _Template, _Options) :-
    throw(Error).
process_event(failure(_ID), _Query, _Template, _Options) :-
    fail.
process_event(prompt(ID, Term), Query, Template, Options) :-
    pengine_output(prompt(ID, Term)),
    wait_event(Query, Template, Options).
process_event(output(ID, Term), Query, Template, Options) :-
    pengine_output(output(ID, Term)),
    pengine_pull_response(ID, Options),
    wait_event(Query, Template, Options).
process_event(success(_ID, Solutions, false), _Query, Template, _Options) :- !,
    member(Template, Solutions).
process_event(success(ID, Solutions, true), Query, Template, Options) :-
    (	member(Template, Solutions)
    ;   pengine_next(ID, Options),
	wait_event(Query, Template, Options)
    ).




/** pengine_ask_around(+URLs, +Query) is nondet

Same as pengine_ask_around(URLs, Query, []).

*/

pengine_ask_around(URLs, Goal) :-
    pengine_ask_around(URLs, Goal, []).


/** pengine_ask_around(+URLs, +Query, +Options) is nondet

Semantically equivalent to Query, except that the   query is run in (and
in the Prolog contexts of)  the  pengine   servers  listed  in URLs, and
(subject to an option) locally. Computes the _bag union_ of solutions to
Query on backtracking.

URLs is a list where each  element  is   either  a  URL (atom) or a pair
URL-Options,  where  Options  is  a   list    of   options  accepted  by
pengine_rpc/3. The options in such a   list override the general options
in Options.

Valid options are:

    * use_local(+Boolean)
      Boolean (=true= or =false=) determines if Query is run (at last)
      also in the local Prolog context.

*/

pengine_ask_around(URLs, Goal, Options) :-
    member(URL0, URLs),
    (   URL0 = URL-OverrideOptions
    ->  true
    ;   URL = URL0,
        OverrideOptions = []
    ),
    merge_options(OverrideOptions, Options, NewOptions),
    catch(pengine_rpc(URL, Goal, NewOptions), _, fail).
pengine_ask_around(_URLs, Goal, Options) :-
    option(use_local(Default), Options, true),
    (   Default
    ->  catch(Goal, _, false)
    ;   fail
    ).



/** pengine_seek_agreement(+URLs, +Query) is nondet

Same as pengine_seek_agreement(URLs, Query, []).

*/

pengine_seek_agreement(URLs, Goal) :-
    pengine_seek_agreement(URLs, Goal, []).



/** pengine_seek_agreement(+URLs, +Query, +Options) is nondet

Semantically equivalent to Query, except that the   query is run in (and
in the Prolog contexts of)  the  pengine   servers  listed  in URLs, and
(subject to an option)  locally.  Computes   the  _bag  intersection_ of
solutions to Query on backtracking.

URLs is a list where each element  is   either  a URL or a `-'-delimited
pair of a URL and  a  list   of  options  accepted by pengine_rpc/3. The
options in such a list override the general options in Options.

Valid options are:

    * use_local(+Boolean)
      Boolean (=true= or =false=) determines if Query is run (at last)
      also in the local Prolog context.

*/

pengine_seek_agreement([], Query, Options) :-
    option(use_local(true), Options), !,
    catch(Query, _, false).
pengine_seek_agreement([], _Query, _Options).
pengine_seek_agreement([URL0|URLs], Query, Options) :-
    (   URL0 = URL-OverrideOptions
    ->  true
    ;   URL = URL0,
        OverrideOptions = []
    ),
    merge_options(OverrideOptions, Options, NewOptions),
    catch(pengine_rpc(URL, Query, NewOptions), _, fail),
    pengine_seek_agreement(URLs, Query, Options).





/*================= HTTP handlers =======================
*/


%   Declare HTTP locations we serve and how.

:- http_handler(root(pengine/create),	     http_pengine_create,	 []).
:- http_handler(root(pengine/send),	     http_pengine_send,		 []).
:- http_handler(root(pengine/pull_response), http_pengine_pull_response, []).
:- http_handler(root(pengine/abort),	     http_pengine_abort,	 []).


http_pengine_create(Request) :-
    allowed(Request),
    http_read_json_dict(Request, Dict),
    (	get_dict(format, Dict, FormatString)
    ->	atom_string(Format, FormatString),
	must_be(oneof([prolog,json,'json-s']), Format)
    ;	Format = prolog
    ),
    dict_to_options(Dict, CreateOptions),
    setting(max_session_pengines, MaxEngines),
    enforce_max_session_pengines(pre, MaxEngines, _),
    message_queue_create(From, []),
    create(From, Pengine, CreateOptions, http),
    enforce_max_session_pengines(post, MaxEngines, Pengine),
    http_pengine_parent(Pengine, Queue),
    wait_and_output_result(Pengine, Queue, Format).

%%	enforce_max_session_pengines(+When, +Max, ?ID) is det.
%
%	Enforce  the  setting  =max_session_pengines=   by  killing  old
%	pengines.
%
%	@tbd	Probably it is cleaner to generate a permission error
%		for creating new pengines.

enforce_max_session_pengines(_When, Max, _) :-
    Max < 0, !.
enforce_max_session_pengines(pre, Max, _) :-
    (	http_in_session(_)
    ->  (   aggregate_all(count, http_session_data(pengine(_ID)), Count),
	    Count >= Max,
	    http_session_retract(pengine(ID))
	->  pengine_destroy(ID)
	;   true
	)
    ;	true
    ).
enforce_max_session_pengines(post, _, ID) :-
    (   http_in_session(_)
    ->	http_session_assert(pengine(ID))
    ;	true
    ).

dict_to_options(Dict, CreateOptions) :-
    dict_pairs(Dict, _, Pairs),
    (	select(probe_template-TemplateString, Pairs, Pairs1),
	select(probe-ProbeString, Pairs1, Pairs2)
    ->	format(string(Combined), '(~s)-(~s)', [TemplateString,ProbeString]),
	term_string(Combined, Template-Probe),
	pairs_create_options(Pairs2, MoreOptions),
	CreateOptions = [probe(Probe), probe_template(Template) | MoreOptions]
    ;	pairs_create_options(Pairs, CreateOptions)
    ).

pairs_create_options([], []).
pairs_create_options([N-V0|T0], [Opt|T]) :-
    Opt =.. [N,V],
    pengine_create_option(Opt), !,
    (   prolog_option(Opt)
    ->  atom_to_term(V0, V, _)
    ;   V = V0
    ),
    pairs_create_options(T0, T).
pairs_create_options([_|T0], T) :-
    pairs_create_options(T0, T).


%%	wait_and_output_result(+Pengine, +Format)
%
%

wait_and_output_result(Pengine, Queue, Format) :-
    setting(time_limit, TimeLimit),
    (   thread_get_message(Queue, Event,
			   [ timeout(TimeLimit)
			   ]),
	debug(pengine(wait), 'Got ~q from ~q', [Event, Queue])
    ->  output_result(Format, Event)
    ;   output_result(Format, error(Pengine,
				    error(time_limit_exceeded, _))),
        pengine_abort(Pengine)
    ).



http_pengine_send(Request) :-
    http_parameters(Request,
		    [ id(ID, [ type(atom) ]),
		      event(EventAtom, []),
		      format(Format, [default(prolog)])
		    ]),
    catch(( atom_to_term(EventAtom, Event0, Bindings),
	    fix_bindings(Format, Event0, ID, Bindings, Event1)
	  ),
	  Error,
	  Event1 = error(ID, Error)),
    (	pengine_thread(ID, Thread)
    ->	http_pengine_parent(ID, Queue),
	thread_send_message(Thread, Event1),
	wait_and_output_result(ID, Queue, Format)
    ;	http_404([], Request)
    ).

fix_bindings(json,
	     request(ask(Goal, Options)), _ID, Bindings,
	     request(ask(Goal, NewOptions))) :- !,
    option(template(Template), Options, Bindings),
    option(paging(Paging), Options, 1),
    NewOptions = [template(Template), paging(Paging)].
fix_bindings('json-s',
	     request(ask(Goal, Options)), _ID, Bindings,
	     request(ask(Goal, NewOptions))) :- !,
    option(template(Template), Options, Bindings),
    option(paging(Paging), Options, 1),
    NewOptions = [template(Template), paging(Paging)].
fix_bindings(_, Command, _, _, Command).

http_pengine_pull_response(Request) :-
    http_parameters(Request,
            [   id(ID, []),
                format(Format, [default(prolog)])
            ]),
    (	http_pengine_parent(ID, Queue)
    ->	wait_and_output_result(ID, Queue, Format)
    ;	http_404([], Request)
    ).

http_pengine_abort(Request) :-
    http_parameters(Request,
            [   id(ID, []),
                format(Format, [default(prolog)])
            ]),
    (	http_pengine_parent(ID, Queue)
    ->	pengine_abort(ID),
	wait_and_output_result(ID, Queue, Format)
    ;	http_404([], Request)
    ).


% Output

output_result(prolog, Term) :-
    to_prolog(Term).
output_result(json, Term) :-
    to_json(Term).
output_result('json-s', Term) :-
    to_json_s(Term).


to_prolog(Term) :-
    format('Content-type: text/x-prolog~n~n'),
    format('~q .~n', [Term]).


to_json(create(ID, Term0)) :-
    term_to_json(Term0, Term),
    reply_json(json([event=create, id=ID, data=Term])).
to_json(stop(ID)) :-
    reply_json(json([event=stop, id=ID])).
to_json(success(ID, Bindings0, More)) :-
    term_to_json(Bindings0, Bindings),
    reply_json(json([event=success, id=ID, data=Bindings, more= @(More)])).
to_json(failure(ID)) :-
    reply_json(json([event=failure, id=ID])).
to_json(error(ID, Error0)) :-
    message_to_string(Error0, Error),
    reply_json(json([event=error, id=ID, data=Error])).
to_json(output(ID, Term0)) :-
    term_to_json(Term0, Json),
    reply_json(json([event=output, id=ID, data=Json])).
to_json(prompt(ID, Term0)) :-
    term_to_json(Term0, Json),
    reply_json(json([event=prompt, id=ID, data=Json])).
to_json(abort(ID)) :-
    reply_json(json([event=abort, id=ID])).
to_json(destroy(ID)) :-
    reply_json(json([event=destroy, id=ID])).


to_json_s(create(ID, Term0)) :-
    term_to_json(Term0, Term),
    reply_json(json([event=create, id=ID, data=Term])).
to_json_s(stop(ID)) :-
    reply_json(json([event=stop, id=ID])).
to_json_s(success(ID, Bindings0, More)) :-
    solution_to_json(Bindings0, Bindings),
    reply_json(json([event=success, id=ID, data=Bindings, more= @(More)])).
to_json_s(failure(ID)) :-
    reply_json(json([event=failure, id=ID])).
to_json_s(error(ID, Error0)) :-
    message_to_string(Error0, Error),
    reply_json(json([event=error, id=ID, data=Error])).
to_json_s(output(ID, Term0)) :-
    term_to_json(Term0, Json),
    reply_json(json([event=output, id=ID, data=Json])).
to_json_s(prompt(ID, Term0)) :-
    term_to_json(Term0, Json),
    reply_json(json([event=prompt, id=ID, data=Json])).
to_json_s(abort(ID)) :-
    reply_json(json([event=abort, id=ID])).
to_json_s(destroy(ID)) :-
    reply_json(json([event=destroy, id=ID])).


solution_to_json(BindingsIn, json(BindingsOut)) :-
    maplist(swap, BindingsIn, BindingsOut).

swap(N=V, N=A) :- term_to_atom(V, A).

%%	allowed(+Request) is det.
%
%	Check whether the peer is allowed to connect.  Returns a
%	=forbidden= header if contact is not allowed.

allowed(Request) :-
	setting(allow_from, Allow),
	match_peer(Request, Allow),
	setting(deny_from, Deny),
	\+ match_peer(Request, Deny), !.
allowed(Request) :-
	memberchk(request_uri(Here), Request),
	throw(http_reply(forbidden(Here))).

match_peer(_, Allowed) :-
	memberchk(*, Allowed), !.
match_peer(_, []) :- !, fail.
match_peer(Request, Allowed) :-
	http_peer(Request, Peer),
	debug(pengine(allow), 'Peer: ~q, Allow: ~q', [Peer, Allowed]),
	(   memberchk(Peer, Allowed)
	->  true
	;   member(Pattern, Allowed),
	    match_peer_pattern(Pattern, Peer)
	).

match_peer_pattern(Pattern, Peer) :-
	ip_term(Pattern, IP),
	ip_term(Peer, IP), !.

ip_term(Peer, Pattern) :-
	split_string(Peer, ".", "", PartStrings),
	ip_pattern(PartStrings, Pattern).

ip_pattern([], []).
ip_pattern([*], _) :- !.
ip_pattern([S|T0], [N|T]) :-
	number_string(N, S),
	ip_pattern(T0, T).


/*================= Built-ins =======================
*/


/** pengine_src_list(+ClauseList) is det

Asserts the list of clauses ClauseList   in the private dynamic database
of  the  current  Pengine.   See   also    the   `src_list'   option  of
pengine_create/1.
*/

pengine_src_list(ClauseList) :-
    maplist(expand_and_assert, ClauseList).


/** pengine_src_text(+SrcText) is det

Asserts the clauses defined in SrcText   in the private dynamic database
of  the  current  Pengine.   See   also    the   `src_text'   option  of
pengine_create/1.

*/

pengine_src_text(Src) :-
    setup_call_cleanup(
	open_chars_stream(Src, Stream),
	read_source(Stream),
	close(Stream)).


/** pengine_src_url(+URL) is det

Asserts the clauses defined in URL in   the  private dynamic database of
the current Pengine. See also the `src_url' option of pengine_create/1.
*/

pengine_src_url(URL) :-
    setup_call_cleanup(
	http_open(URL, Stream, []),
	read_source(Stream),
	close(Stream)).

read_source(Stream) :-
    read(Stream, Term),
    read_source(Term, Stream).

read_source(end_of_file, _Stream) :- !.
read_source(Term, Stream) :-
    expand_and_assert(Term),
    read_source(Stream).


expand_and_assert(Term) :-
    expand_term(Term, ExpandedTerm),
    (   is_list(ExpandedTerm)
    ->  maplist(assert_local, ExpandedTerm)
    ;   assert_local(ExpandedTerm)
    ).


assert_local(:-(Head, Body)) :- !,
    functor(Head, F, N),
    thread_local(F/N),
    assert(:-(Head, Body)).
assert_local(:-Body) :- !,
    (   safe_goal(Body)
    ->  call(Body)
    ;   true
    ).
assert_local(Fact) :-
    functor(Fact, F, N),
    thread_local(F/N),
    assert(Fact).


/*================= Utilities =======================
*/

/** pengine_find_n(+N, ?Template, +Goal, ?List) is nondet

Acts as findall/3 but returns only the   first N bindings of Template to
List, on backtracking another batch of N bindings, and so on.
*/

pengine_find_n(N, Template, Goal, List) :-
    copy_term(Template-Goal, TemplateCopy-GoalCopy),
    Counter = counter(0),
    (   call(GoalCopy),
        arg(1, Counter, N1),
        N2 is N1 + 1,
        nb_setarg(1, Counter, N2),
        recordz(sols, TemplateCopy),
        N2 == N,
        nb_setarg(1, Counter, 0)
    ;   true
    ),
    findall(Row, (recorded(sols, Row, Ref), erase(Ref)), List),
    List \= [].
