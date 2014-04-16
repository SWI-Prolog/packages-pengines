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
            pengine_ask/3,			% +Pengine, :Query, +Options
            pengine_next/2,			% +Pengine. +Options
            pengine_stop/2,			% +Pengine. +Options
            pengine_event/2,			% -Event, +Options
            pengine_input/2,			% +Prompt, -Term
            pengine_output/1,			% +Term
            pengine_output/2,			% +Term, +Options
            pengine_respond/3,			% +Pengine, +Input, +Options
            pengine_debug/2,			% +Format, +Args
            pengine_self/1,			% -Pengine
	    pengine_name/2,                     % ?Pengine, ?Name
            pengine_pull_response/2,		% +Pengine, +Options
            pengine_destroy/1,			% +Pengine
            pengine_abort/1,			% +Pengine
	    pengine_application/1,		% +Application
            pengine_property/2,			% ?Pengine, ?Property
            pengine_event_loop/2,		% :Closure, +Options
            pengine_rpc/2,			% +Server, :Goal
            pengine_rpc/3,			% +Server, +Goal, +Options
            pengine_ask_around/3,		% +list(Server), :Goal, +Options
            pengine_seek_agreement/3		% +list(Server), :Goal, +Options
	  ]).

/** <module> Pengines: Web Logic Programming Made Easy

The library(pengines) provides an  infrastructure   for  creating Prolog
engines in a (remote) pengine server  and accessing these engines either
from Prolog or JavaScript.

@author Torbjörn Lager and Jan Wielemaker
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_cors)).
:- use_module(library(thread_pool)).
:- use_module(library(aggregate)).
:- use_module(library(uri)).
:- use_module(library(filesex)).
:- use_module(library(time)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(apply)).
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
	pengine_event_loop(1, +),
	pengine_ask_around(+, 0, +),
	pengine_seek_agreement(+, 0, +).

:- predicate_options(pengine_create/1, 1,
		     [ id(-atom),
		       name(atom),
		       server(atom),
		       ask(compound),
		       template(compound),
		       chunk(integer),
		       destroy(boolean),
		       application(atom),
		       src_list(list),
		       src_text(any),		% text
		       src_url(atom),
		       src_predicates(list),
		       format(oneof([prolog,json,'json-s']))
		     ]).
:- predicate_options(pengine_ask/3, 3,
		     [ template(any),
		       chunk(integer)
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
:- predicate_options(pengine_respond/3, 2,
		     [ pass_to(pengine_send/3, 3)
		     ]).
:- predicate_options(pengine_rpc/3, 3,
		     [ chunk(integer),
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
:- debug(pengine(debug)).		% handle pengine_debug in pengine_rpc/3.


:- meta_predicate			% internal meta predicates
	solve(?, 0, +, +),
	pengine_event_loop(1, +, +),
	pengine_event_loop(+, 1, +, +),
	pengine_find_n(+, ?, 0, -).

/**  pengine_create(:Options) is det.

    Creates a new pengine. Valid options are:

    * id(-ID)
      ID gets instantiated to the id of the pengine.  ID is an atom
      (a UUID).

    * name(+Name)
      The pengine is named Name (an atom). A slave pengine (child) can
      subsequently be referred to by this name, but only by its master
      (parent). The atoms =parent= and =self= are reserved names and
      must not be used here.

    * application(+Name)
      The pengine will run in (and in the Prolog context of) the pengine
      application Name.

    * server(+URL)
      The pengine will run in (and in the Prolog context of) the pengine
      server located at URL.

    * ask(@Query)
      Make Query the _first_ query to be solved by this pengine (and thus
      avoid one network round-trip).

    * template(+Template)
      Template is a variable (or a term containing variables) shared
      with the query. By default, the template is identical to the
      query. Meaningful only if the ask(Query) option is used, and
      valid only for that query.

    * chunk(+Integer)
      Retrieve solutions in chunks of Integer rather than one by one. 1
      means no chunking (default). Other integers indicate the maximum
      number of solutions to retrieve in one chunk. Meaningful only if
      the ask(Query) option is used, and valid only for that query.

    * destroy(+Boolean)
      Destroy the pengine after the completion (including backtracking)
      of the processing of the first query. Defaults to =true=.

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

    * format(+Format)
      Determines the format of event responses. Format is an atom,
      either =prolog= (default), =json=, or =json-s=.

Remaining  options  are  passed  to  http_open/3  (meaningful  only  for
non-local pengines) and thread_create/3. Note   that for thread_create/3
only options changing the stack-sizes can be used. In particular, do not
pass the detached or alias options.

Successful creation of a pengine will return an _event term_ of the
following form:

    * create(ID, Term)
      ID is the id of the pengine that was created.
      In case the ask(Query) option is used, Term
      is bound to a list of features revealing data
      about the pengine server.

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
    pengine_self(Self),
    pengine_parent(Queue),
    (   retract(wrap_first_answer_in_create_event)
    ->  get_pengine_application(Self, Application),
        setting(Application:slave_limit, Max),
        Event = create(Self, [answer=output(Self, Event0), slave_limit=Max])
    ;   Event = output(Self, Event0)
    ),
    delay_message(queue(Queue), Event, Options).
pengine_send2(self, Event, Options) :- !,
    thread_self(Queue),
    delay_message(queue(Queue), Event, Options).
pengine_send2(Name, Event, Options) :-
    pengine_name(Target, Name), !,
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
    ;	existence_error(pengine, Pengine)
    ).

%%	pengine_reply(+Event) is det.
%%	pengine_reply(+Queue, +Event) is det.
%
%	Reply Event to the parent of the   current  Pengine or the given
%	Queue.

pengine_reply(Event) :-
    nb_getval(pengine_parent, Queue),
    debug(pengine(event), 'Reply to ~p: ~p', [Queue, Event]),
    pengine_reply(Queue, Event).

pengine_reply(Queue, Event) :-
    retract(wrap_first_answer_in_create_event), !,
    pengine_self(Self),
    get_pengine_application(Self, Application),
    setting(Application:slave_limit, Max),
    thread_send_message(Queue, create(Self, [answer=Event, slave_limit=Max])).
pengine_reply(Queue, Event) :-
    thread_send_message(Queue, Event).

/** pengine_ask(+NameOrID, @Query, +Options) is det

Asks pengine NameOrID a query Query.

Options is a list of options:

    * template(+Template)
      Template is a variable (or a term containing variables) shared
      with the query. By default, the template is identical to the
      query.

    * chunk(+Integer)
      Retrieve solutions in chunks of Integer rather than one by one. 1
      means no chunking (default). Other integers indicate the maximum
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
      pengine_next/2.

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
      ID is the id of the pengine that called pengine_input/2 and Term is
      the prompt.

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
pengine_ask_option(chunk(_)).


/** pengine_next(+NameOrID, +Options) is det

Asks pengine NameOrID for the next solution to a query started by
pengine_ask/3. Options are passed to pengine_send/3.

Here too, results will be returned in the form of _event terms_.

    * success(ID, Terms, More)
      ID is the id of the pengine that succeeded in finding yet another
      solution to the query. Terms is a list holding instantiations of
      `Template`. More is either `true` or `false`, indicating whether
      we can expect the pengine to be able to return more solutions or
      not, would we call pengine_next/2.

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
      ID is the id of the pengine that called pengine_input/2 and Term
      is the prompt.

Defined in terms of pengine_send/3, as follows:

==
pengine_next(ID, Options) :-
    pengine_send(ID, request(next), Options).
==

*/

pengine_next(Pengine, Options) :-
	pengine_send(Pengine, request(next), Options).


/** pengine_stop(+NameOrID, +Options) is det

Tells pengine NameOrID to stop looking  for   more  solutions to a query
started by pengine_ask/3. Options are passed to pengine_send/3.

Defined in terms of pengine_send/3, like so:

==
pengine_stop(ID, Options) :-
    pengine_send(ID, request(stop), Options).
==
*/

pengine_stop(Pengine, Options) :-
	pengine_send(Pengine, request(stop), Options).


/** pengine_abort(+NameOrID) is det

Aborts the running query. The pengine goes   back  to state `2', waiting
for new queries.

@see pengine_destroy/1.
*/

pengine_abort(Pengine) :-
    pengine_remote(Pengine, Server), !,
    remote_pengine_abort(Server, Pengine, []).
pengine_abort(Pengine) :-
    pengine_thread(Pengine, Thread),
    catch(thread_signal(Thread, throw(abort_query)), _, true).


/** pengine_destroy(+NameOrID) is det

Destroys the pengine NameOrID.

*/

pengine_destroy(Pengine) :-
    catch(pengine_send(Pengine, request(destroy)),
	  error(existence_error(pengine, Pengine), _),
	  true),
    retractall(pengine_name(Pengine, _)).



%%	pengine_name(?Pengine, ?Name)
%
%	Local predicate that relates Pengine to a Name.
%	Accessible only from the master of Pengine.
%	Fails if Pengine has not been named.

:- thread_local
	pengine_name/2.			% ?Pengine, ?Name



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
	current_pengine/6.		% Id, ParentId, Thread, URL
:- volatile
	current_pengine/6.

:- thread_local
	child/1.                % ?Child



%%	pengine_register_local(-Id, +Thread, +Queue, +URL, +Application) is det.
%%	pengine_register_remote(+Id, +URL, +Queue) is det.
%%	pengine_unregister(+Id) is det.

pengine_register_local(Id, Thread, Queue, URL, Application, Destroy) :-
    uuid(Id),
    asserta(current_pengine(Id, Queue, Thread, URL, Application, Destroy)).

pengine_register_remote(Id, URL) :-
    thread_self(Queue),
    asserta(current_pengine(Id, Queue, 0, URL, 0, 0)).

pengine_unregister(Id) :-
    retractall(current_pengine(Id, _, _, _, _, _)),
    retractall(pengine_name(Id, _)).

pengine_self(Id) :-
    thread_self(Thread),
    current_pengine(Id, _Parent, Thread, _URL, _Application, _Destroy).

pengine_parent(Parent) :-
    nb_getval(pengine_parent, Parent).

http_pengine_parent(Pengine, Parent) :-
    current_pengine(Pengine, Parent, Thread, _URL, _Application, _Destroy),
    Thread \== 0, !.

pengine_thread(Pengine, Thread) :-
    current_pengine(Pengine, _Parent, Thread, _URL, _Application, _Destroy),
    Thread \== 0, !.

pengine_remote(Pengine, URL) :-
    current_pengine(Pengine, _Parent, 0, URL, _Application, _Destroy).

get_pengine_application(Pengine, Application) :-
    current_pengine(Pengine, _Parent, Thread, _URL, Application, _Destroy),
    Thread \== 0, !.

get_pengine_destroy(Pengine, Destroy) :-
    current_pengine(Pengine, _Parent, _Thread, _URL, _Application, Destroy).


:- if(\+current_predicate(uuid/1)).
:- use_module(library(random)).
uuid(Id) :-
    Max is 1<<128,
    random_between(0, Max, Num),
    atom_number(Id, Num).
:- endif.


/** pengine_application(+Application) is det.

Directive that must be used to declarate  a module a pengine application
module. The module may not  be  associated   to  any  file.  The default
application is =pengine_sandbox=.  The  example   below  creates  a  new
application =address_book= and imports the  API   defined  in the module
file =adress_book_api.pl= into the application.

  ==
  :- pengine_application(address_book).
  :- use_module(address_book:adress_book_api).
  ==
*/

pengine_application(Application) :-
    throw(error(context_error(nodirective,
			      pengine_application(Application)), _)).

:- multifile
    system:term_expansion/2,
    current_application/1.

% Default settings for all applications

:- setting(thread_pool_size, integer, 100,
	   'Maximum number of pengines this application can run.').
:- setting(thread_pool_stacks, list(compound), [],
	   'Maximum stack sizes for pengines this application can run.').
:- setting(slave_limit, integer, 3,
	   'Maximum number of local slave pengines a master pengine can create.').
:- setting(time_limit, number, 30,
	   'Maximum time to wait for output').
:- setting(allow_from, list(atom), [*],
	   'IP addresses from which remotes are allowed to connect').
:- setting(deny_from, list(atom), [],
	   'IP addresses from which remotes are NOT allowed to connect').


system:term_expansion((:- pengine_application(Application)), Expanded) :-
    must_be(atom, Application),
    (   module_property(Application, file(_))
    ->  permission_error(create, pengine_application, Application)
    ;   true
    ),
    expand_term((:- setting(Application:thread_pool_size, integer, setting(pengine:thread_pool_size),
			    'Maximum number of pengines this application can run.')),
		ThreadPoolSizeSetting),
    expand_term((:- setting(Application:thread_pool_stacks, list(compound), setting(pengine:thread_pool_stacks),
			    'Maximum stack sizes for pengines this application can run.')),
		ThreadPoolStacksSetting),
    expand_term((:- setting(Application:slave_limit, integer, setting(pengine:slave_limit),
			    'Maximum number of local slave pengines a master pengine can create.')),
		SlaveLimitSetting),
    expand_term((:- setting(Application:time_limit, number, setting(pengine:time_limit),
			    'Maximum time to wait for output')),
		TimeLimitSetting),
    expand_term((:- setting(Application:allow_from, list(atom), setting(pengine:allow_from),
			    'IP addresses from which remotes are allowed to connect')),
		AllowFromSetting),
    expand_term((:- setting(Application:deny_from, list(atom), setting(pengine:deny_from),
			    'IP addresses from which remotes are NOT allowed to connect')),
		DenyFromSetting),
    flatten([ pengine:current_application(Application),
	      ThreadPoolSizeSetting,
	      ThreadPoolStacksSetting,
	      SlaveLimitSetting,
	      TimeLimitSetting,
	      AllowFromSetting,
	      DenyFromSetting
	    ], Expanded).


% Register default application

:- pengine_application(pengine_sandbox).


/** pengine_property(+NameOrID, ?Property) is nondet.

True  when  Property  is  a  property  of  the  given  Pengine.  Defined
properties are:

  * parent(Thread)
    Thread id for the parent (local) pengine.
  * self(Thread)
    Thread id of the running pengine.
  * remote(Server)
    Pengine runs on the remote Server.
  * application(Application)
    Pengine runs the given application
*/


pengine_property(Id, parent(Parent)) :-
    current_pengine(Id, Parent, _Thread, _URL, _Application, _Destroy).
pengine_property(Id, self(Id)) :-
    current_pengine(Id, _Parent, _Thread, _URL, _Application, _Destroy).
pengine_property(Id, remote(Server)) :-
    current_pengine(Id, _Parent, 0, Server, _Application, _Destroy).
pengine_property(Id, application(Application)) :-
    current_pengine(Id, _Parent, _Thread, _Server, Application, _Destroy).


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


/** pengine_debug(+Format, +Args) is det

Create a message using format/3 from Format   and  Args and send this to
the    client.    The    default    JavaScript    client    will    call
=|console.log(Message)|=  if  there  is   a    console.   The  predicate
pengine_rpc/3 calls debug(pengine(debug), '~w',   [Message]).  The debug
topic pengine(debug) is enabled by default.

@see debug/1 and nodebug/1 for controlling the pengine(debug) topic
@see format/2 for format specifications
*/

pengine_debug(Format, Args) :-
    pengine_parent(Queue),
    pengine_self(Self),
    catch(safe_goal(format(atom(_), Format, Args)), E, true),
    (	var(E)
    ->	format(atom(Message), Format, Args)
    ;	message_to_string(E, Message)
    ),
    thread_send_message(Queue, debug(Self, Message)).


/*================= Local pengine =======================
*/

%%	local_pengine_create(+Options)
%
%	Creates  a  local   Pengine,   which    is   a   thread  running
%	pengine_main/3.  It maintains two predicates:
%
%	  - The global dynamic predicate child/1 relates Pengines to their
%	    childs.
%	  - The local predicate pengine_name/2 maps childs to their names.

local_pengine_create(Options) :-
    thread_self(Self),
    option(application(Application), Options, pengine_sandbox),
    catch(create(Self, Child, Options, local, Application), Error, true),
    (   var(Error)
    ->  assert(child(Child)),
        (   option(name(Name), Options)
        ->  assert(pengine_name(Child, Name))
        ;   true
        )
    ;   message_to_string(Error, ErrorString),
        setting(Application:slave_limit, Max),
        pengine_reply(create(null, [ answer=error(null, ErrorString),
				     slave_limit=Max
				   ]))
    ).


%%	create(+Queue, -Child, +Options, +URL, +Application) is det.
%
%	Create a new pengine thread.
%
%	@arg Queue is the queue (or thread handle) to report to
%	@arg Child is the identifier of the created pengine.

thread_pool:create_pool(Application) :-
    current_application(Application),
    setting(Application:thread_pool_size, Size),
    setting(Application:thread_pool_stacks, Stacks),
    thread_pool_create(Application, Size, Stacks).


create(Queue, Child, Options, URL, Application) :-
    (	current_application(Application)
    ->	true
    ;	existence_error(pengine_application, Application)
    ),
    aggregate_all(count, child(_), Count),
    setting(Application:slave_limit, Max),
    (   Count >= Max
    ->  pengine_done,
        throw(error(resourc_error(max_pengines, _)))
    ;   partition(pengine_create_option, Options, PengineOptions, RestOptions),
        thread_create_in_pool(
            Application,
            pengine_main(Queue, PengineOptions, Application), ChildThread,
            [ wait(false),
	      at_exit(pengine_done)
	    | RestOptions
	    ]),
	option(destroy(Destroy), PengineOptions, true),
        pengine_register_local(Child, ChildThread, Queue, URL, Application, Destroy),
        thread_send_message(ChildThread, pengine_registered(Child)),
        (    option(id(Id), Options)
        ->   Id = Child
        ;    true
        )
    ).


pengine_create_option(src_text(_)).
pengine_create_option(src_list(_)).
pengine_create_option(src_url(_)).
pengine_create_option(src_predicates(_)).
pengine_create_option(ask(_)).
pengine_create_option(template(_)).
pengine_create_option(chunk(_)).
pengine_create_option(destroy(_)).
pengine_create_option(application(_)).



%%	pengine_done is det.
%
%	Called  from  the  pengine  thread  =at_exit=  option.  Destroys
%	_child_ pengines using pengine_destroy/1.

:- public
	pengine_done/0.

pengine_done :-
    forall(retract(child(Child)), pengine_destroy(Child)),
    pengine_self(Id),
    (	\+ current_pengine(Id, _, _, http, _, _)
    ->	pengine_unregister(Id)
    ;	true
    ).


%%	pengine_main(+Parent, +Options, +Application)
%
%	Run a pengine main loop. First acknowledges its creation and run
%	pengine_main_loop/1.

:- thread_local wrap_first_answer_in_create_event/0.

pengine_main(Parent, Options, Application) :-
    fix_streams,
    thread_get_message(pengine_registered(Self)),
    nb_setval(pengine_parent, Parent),
    (   catch(maplist(process_create_option(Application), Options), Error,
	      ( send_error(Error),
		fail
	      ))
    ->  (   option(ask(Query), Options)
        ->  asserta(wrap_first_answer_in_create_event),
	    option(template(Template), Options, Query),
	    option(chunk(Chunk), Options, 1),
	    pengine_ask(Self, Query, [template(Template), chunk(Chunk)])
	;   setting(Application:slave_limit, Max),
	    pengine_reply(create(Self, [slave_limit=Max]))
        ),
        get_pengine_destroy(Self, Destroy),
        pengine_main_loop(Self, Destroy)
    ;   pengine_terminate(Self)
    ).

%%	fix_streams is det.
%
%	If we are a pengine that is   created  from a web server thread,
%	the current output points to a CGI stream.

fix_streams :-
    catch(fix_stream(current_output), _, true).

fix_stream(Name) :-
    is_cgi_stream(Name), !,
    debug(pengine(stream), '~w is a CGI stream!', [Name]),
    set_stream(user_output, alias(Name)).
fix_stream(_).


process_create_option(Application, src_list(ClauseList)) :- !,
    pengine_src_list(Application, ClauseList).
process_create_option(Application, src_text(Text)) :- !,
    pengine_src_text(Application, Text).
process_create_option(Application, src_url(URL)) :- !,
    pengine_src_url(Application, URL).
process_create_option(_Application, _).


pengine_main_loop(ID, Destroy) :-
    catch(guarded_main_loop(ID, Destroy), abort_query,
	  ( pengine_reply(abort(ID)),
	    pengine_main_loop(ID, Destroy)
	  )).


%%	guarded_main_loop(+Pengine, +Destroy) is det.
%
%	Executes state `2' of  the  pengine,   where  it  waits  for two
%	events:
%
%	  - destroy
%	  Terminate the pengine
%	  - ask(:Goal, +Options)
%	  Solve Goal.

guarded_main_loop(ID, Destroy) :-
    pengine_event(Event),
    (   Event = request(destroy)
    ->  debug(pengine(transition), '~q: 2 = ~q => 1', [ID, destroy]),
	pengine_terminate(ID)
    ;   Event = request(ask(Goal, Options))
    ->  debug(pengine(transition), '~q: 2 = ~q => 3', [ID, ask(Goal)]),
        ask(ID, Goal, Options, Destroy)
    ;   debug(pengine(transition), '~q: 2 = ~q => 2', [ID, protocol_error]),
        %pengine_reply(error(ID, error(protocol_error, _))),
        guarded_main_loop(ID, Destroy)
    ).


pengine_terminate(ID) :-
    pengine_reply(destroy(ID)),
    thread_self(Self),		% Make the thread silently disappear
    thread_detach(Self).


%%	solve(+Template, :Goal, +ID, +Destroy) is det.
%
%	Solve Goal. Note that because we can ask for a new goal in state
%	`6', we must provide for an ancesteral cut (prolog_cut_to/1). We
%	need to be sure to  have  a   choice  point  before  we can call
%	prolog_current_choice/1. This is the reason   why this predicate
%	has two clauses.

solve(Template, Goal, ID, Destroy) :-
    prolog_current_choice(Choice),
    (   call_cleanup(catch(Goal, Error, true), Det=true),
        (   var(Error)
        ->  (   var(Det)
            ->  pengine_reply(success(ID, Template, true)),
                more_solutions(ID, Choice, Destroy)
            ;   !,			% commit
                destroy_or_continue(Destroy, ID, success(ID, Template, false))
            )
        ;   !,				% commit
            destroy_or_continue(Destroy, ID, error(ID, Error))
        )
    ;   !,				% commit
        destroy_or_continue(Destroy, ID, failure(ID))
    ).
solve(_, _, _, _).			% leave a choice point



destroy_or_continue(Destroy, ID, Event) :-
    (   Destroy == true
    ->  pengine_reply(destroy(ID, Event)),
        thread_self(Self),
        thread_detach(Self)
    ;   pengine_reply(Event),
        guarded_main_loop(ID, true)
    ).



%%	more_solutions(+Pengine, +Choice)
%
%	Called after a solution was found while  there can be more. This
%	is state `6' of the state machine. It processes these events:
%
%	  * stop
%	  Go back via state `7' to state `2' (guarded_main_loop/1)
%	  * next
%	  Fail.  This causes solve/4 to backtrack on the goal asked.
%	  * ask(Goal, Options)
%	  Ask another goal.  Note that we must commit the choice point
%	  of the previous goal asked for.

more_solutions(ID, Choice, Destroy) :-
    pengine_event(request(Event)),
    more_solutions(Event, ID, Choice, Destroy).

more_solutions(stop, ID, _Choice, Destroy) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 7', [ID, stop]),
    destroy_or_continue(Destroy, ID, stop(ID)).
more_solutions(next, ID, _Choice, _Destroy) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, next]),
    fail.
more_solutions(ask(Goal, Options), ID, Choice, Destroy) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, ask(Goal)]),
    prolog_cut_to(Choice),
    ask(ID, Goal, Options, Destroy).
more_solutions(destroy, ID, _Choice, _Destroy) :- !,
    debug(pengine(transition), '~q: 6 = ~q => 1', [ID, destroy]),
    pengine_terminate(ID).
more_solutions(Event, ID, Choice, Destroy) :-
    debug(pengine(transition), '~q: 6 = ~q => 6', [ID, protocol_error(Event)]),
    pengine_reply(error(ID, error(protocol_error, _))),
    more_solutions(ID, Choice, Destroy).


%%	ask(+Pengine, :Goal, +Options, +Destroy)
%
%	Migrate from state `2' to `3'.  This predicate validates that it
%	is safe to call Goal using safe_goal/1 and then calls solve/4 to
%	prove the goal. It also takes care of the chunk(N) option.

ask(ID, Goal, Options, Destroy) :-
    get_pengine_application(ID, Application),
    expand_goal(Application:Goal, Goal1),
    catch(safe_goal(Goal1), Error, true),
    (   var(Error)
    ->  option(template(Template), Options, Goal),
        option(chunk(N), Options, 1),
        (   N == 1
        ->  solve([Template], Goal1, ID, Destroy)
        ;   solve(Res, pengine_find_n(N, Template, Goal1, Res), ID, Destroy)
        )
    ;   pengine_reply(error(ID, Error)),
	guarded_main_loop(ID, Destroy)
    ).


/** pengine_pull_response(+Pengine, +Options) is det

Pulls a response (an event term) from the  slave Pengine if Pengine is a
remote process, else does nothing at all.
*/

pengine_pull_response(Pengine, Options) :-
    pengine_remote(Pengine, Server), !,
    remote_pengine_pull_response(Server, Pengine, Options).
pengine_pull_response(_ID, _Options).


/** pengine_input(+Prompt, -Term) is det

Sends Prompt to the parent pengine and waits for input. Note that Prompt may be
anÃÂ¿ term, atomic or complex.
*/

pengine_input(Prompt, Term) :-
    pengine_self(Self),
    nb_getval(pengine_parent, Parent),
    pengine_reply(Parent, prompt(Self, Prompt)),
    pengine_event(input(Term)).


/** pengine_respond(+Pengine, +Input, +Options) is det

Sends a response in the form of the term Input to a slave pengine
that has prompted its master for input.

Defined in terms of pengine_send/3, as follows:

==
pengine_respond(Pengine, Input, Options) :-
    pengine_send(Pengine, input(Input), Options).
==

*/

pengine_respond(Pengine, Input, Options) :-
    pengine_send(Pengine, input(Input), Options).


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
    assert(child(ID)),
    (   option(name(Name), Options)
    ->  assert(pengine_name(ID, Name))
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
    copy_term(Options, Options1),
    numbervars(Options1, 0, _, [singletons(true)]),
    maplist(prolog_option, Options1, Options2),
    dict_create(Dict, _, Options2).

prolog_option(Option0, Option) :-
    prolog_option(Option0), !,
    Option0 =.. [Name, Value],
    format(string(String), '~W',
	   [Value, [ignore_ops(true), quoted(true), numbervars(true)]]),
    Option =.. [Name, String].
prolog_option(Option, Option).

prolog_option(src_list(_)).
prolog_option(ask(_)).
prolog_option(template(_)).
prolog_option(application(_)).
prolog_option(destroy(_)).


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


/** pengine_event_loop(:Closure, +Options) is det

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
     Forwards received event terms to slaves.  The To argument
     is reserved for future extensions and must be instantiated to
     =all=.

   * created(+List)
     Initial list of pengine IDs to process.  This may be needed if
     events from more than one pengines must be processed because the
     event loop may otherwise terminate after processing all events of
     the first pengine because subsequent pengines have not yet become
     visible.
*/

pengine_event_loop(Closure, Options) :-
    option(created(Created), Options, []),
    pengine_event_loop(Closure, Created, Options).

pengine_event_loop(Closure, Created, Options) :-
    pengine_event(Event),
    debug(pengine(event), 'EVENT: ~q', [Event]),
    (   option(autoforward(all), Options)
    ->  forall(member(ID, Created), pengine_send(ID, Event))
    ;   true
    ),
    pengine_event_loop(Event, Closure, Created, Options).

pengine_event_loop(create(ID, Features), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 1 = /~q => 2', [ID, create(Features)]),
    ignore(call(Closure, create(ID, Features))),
    (	memberchk(ID, Created)
    ->	Created2 = Created
    ;	Created2 = [ID|Created]
    ),
    (   option(answer(Answer), Features)
    ->  pengine_event_loop(Answer, Closure, Created2, Options)
    ;   pengine_event_loop(Closure, Created2, Options)
    ).
pengine_event_loop(output(ID, Msg), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 4', [ID, output(Msg)]),
    ignore(call(Closure, output(ID, Msg))),
    pengine_pull_response(ID, []),
    pengine_event_loop(Closure, Created, Options).
pengine_event_loop(debug(ID, Msg), Closure, Created, Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 4', [ID, debug(Msg)]),
    ignore(call(Closure, debug(ID, Msg))),
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
pengine_event_loop(destroy(ID, Event), Closure, Created, Options) :-
    ignore(call(Closure, Event)),
    pengine_event_loop(destroy(ID), Closure, Created, Options).
pengine_event_loop(destroy(ID), Closure, Created, Options) :-
    ignore(call(Closure, destroy(ID))),
    retractall(child(ID)),
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

    * chunk(+Integer)
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
			 id(Id),
			 destroy(false)
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

process_event(create(ID, _Features), Query, Template, Options) :-
    pengine_ask(ID, Query, [template(Template)|Options]),
    wait_event(Query, Template, Options).
process_event(error(_ID, Error), _Query, _Template, _Options) :-
    throw(Error).
process_event(failure(_ID), _Query, _Template, _Options) :-
    fail.
process_event(prompt(ID, Prompt), Query, Template, Options) :-
    pengine_rpc_prompt(ID, Prompt, Reply),
    pengine_send(ID, input(Reply)),
    wait_event(Query, Template, Options).
process_event(output(ID, Term), Query, Template, Options) :-
    pengine_rpc_output(ID, Term),
    pengine_pull_response(ID, Options),
    wait_event(Query, Template, Options).
process_event(debug(ID, Message), Query, Template, Options) :-
    debug(pengine(debug), '~w', [Message]),
    pengine_pull_response(ID, Options),
    wait_event(Query, Template, Options).
process_event(success(_ID, Solutions, false), _Query, Template, _Options) :-
    member(Template, Solutions).
process_event(success(ID, Solutions, true), Query, Template, Options) :-
    (	member(Template, Solutions)
    ;   pengine_next(ID, Options),
	wait_event(Query, Template, Options)
    ).

pengine_rpc_prompt(ID, Prompt, Term) :-
    prompt(ID, Prompt, Term0), !,
    Term = Term0.
pengine_rpc_prompt(_ID, Prompt, Term) :-
    setup_call_cleanup(
	prompt(Old, Prompt),
	read(Term),
	prompt(_, Old)).

pengine_rpc_output(ID, Term) :-
    output(ID, Term), !.
pengine_rpc_output(_ID, Term) :-
    print(Term).


%%  prompt(+ID, +Prompt, -Term) is semidet.
%
%   Hook to handle pengine_input/2 from the remote pengine. If the hooks
%   fails, pengine_rpc/3 calls read/1 using the current prompt.

:- multifile prompt/3.

%%  output(+ID, +Term) is semidet.
%
%   Hook to handle pengine_output/1 from the remote pengine. If the hook
%   fails, it calls print/1 on Term.

:- multifile output/2.


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
:- http_handler(root(pengine/destroy),	 http_pengine_destroy,	 []).
:- http_handler(root(pengine/destroy_all),	 http_pengine_destroy_all,	 []).


http_pengine_create(Request) :-
    http_read_json_dict(Request, Dict),
    (	get_dict(format, Dict, FormatString)
    ->	atom_string(Format, FormatString),
	    must_be(oneof([prolog,json,'json-s']), Format)
    ;	Format = prolog
    ),
    dict_to_options(Dict, CreateOptions),
    option(application(Application), CreateOptions, pengine_sandbox),
    allowed(Request, Application),
    message_queue_create(From, []),
    catch(create(From, Pengine, CreateOptions, http, Application), Error, true),
    (   var(Error)
    ->  http_pengine_parent(Pengine, Queue),
        wait_and_output_result(Pengine, Queue, Format)
    ;   message_to_string(Error, ErrorString),
        setting(Application:slave_limit, Max),
        output_result(Format, create(null,
				     [ answer=error(null, ErrorString),
				       slave_limit=Max
				     ]))
    ).


dict_to_options(Dict, CreateOptions) :-
    dict_pairs(Dict, _, Pairs),
    pairs_create_options(Pairs, CreateOptions).

pairs_create_options([], []).
pairs_create_options(T0, [AskOpt, TemplateOpt|T]) :-
    select(ask-Ask, T0, T1),
    select(template-Template, T1, T2), !,
    atomic_list_concat([Ask, -, Template], AskTemplate),
    atom_to_term(AskTemplate, Ask1-Template1, _),
    AskOpt = ask(Ask1),
    TemplateOpt = template(Template1),
    pairs_create_options(T2, T).
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


%%	wait_and_output_result(+Pengine, +Queue, +Format)
%
%	Wait for the Pengine's Queue and if  there is a message, send it
%	to the requestor using  output_result/1.   If  Pengine  does not
%	answer within the time specified   by  the setting =time_limit=,
%	Pengine is aborted and the  result is error(time_limit_exceeded,
%	_).

wait_and_output_result(Pengine, Queue, Format) :-
    get_pengine_application(Pengine, Application),
    setting(Application:time_limit, TimeLimit),
    (   thread_get_message(Queue, Event,
			   [ timeout(TimeLimit)
			   ]),
	debug(pengine(wait), 'Got ~q from ~q', [Event, Queue])
    ->  output_result(Format, Event),
	(	get_pengine_destroy(Pengine, true)
	->	maybe_unregister_pengine(Event)
	;	true
	)
    ;   output_result(Format, error(Pengine,
				    error(time_limit_exceeded, _))),
        pengine_abort(Pengine)
    ).

maybe_unregister_pengine(success(Pengine, _, false)) :- !,
	pengine_unregister(Pengine).
maybe_unregister_pengine(failure(Pengine)) :- !,
	pengine_unregister(Pengine).
maybe_unregister_pengine(destroy(Pengine)) :- !,
	pengine_unregister(Pengine).
maybe_unregister_pengine(destroy(Pengine, _)) :- !,
	pengine_unregister(Pengine).
maybe_unregister_pengine(error(Pengine, _)) :- !,
	pengine_unregister(Pengine).
maybe_unregister_pengine(_).


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
    option(chunk(Paging), Options, 1),
    NewOptions = [template(Template), chunk(Paging)].
fix_bindings('json-s',
	     request(ask(Goal, Options)), _ID, Bindings,
	     request(ask(Goal, NewOptions))) :- !,
    option(template(Template), Options, Bindings),
    option(chunk(Paging), Options, 1),
    NewOptions = [template(Template), chunk(Paging)].
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


http_pengine_destroy(Request) :-
    http_parameters(Request,
            [   id(ID, []),
                format(Format, [default(prolog)])
            ]),
    (	http_pengine_parent(ID, Queue)
    ->	pengine_destroy(ID),
	wait_and_output_result(ID, Queue, Format)
    ;	http_404([], Request)
    ).

http_pengine_destroy_all(Request) :-
    http_parameters(Request,
            [   ids(IDsAtom, [])
            ]),
    atomic_list_concat(IDs, ',', IDsAtom),
    maplist(pengine_destroy_hard, IDs),
    reply_json("ok").

pengine_destroy_hard(Pengine) :-
    pengine_thread(Pengine, Thread),
    catch(thread_signal(Thread, abort), _, true),
    catch(thread_join(Thread, _), _, true),
    pengine_unregister(Pengine).



% Output

output_result(prolog, Term) :-
    to_prolog(Term).
output_result(json, Term) :-
    cors_enable,
    to_json(Term).
output_result('json-s', Term) :-
    cors_enable,
    to_json_s(Term).

%%	to_prolog(+Term)
%
%	Send a term in Prolog  syntax.   Should  we  use cycles(true) as
%	well? What about attributes? Use copy_term/3?

to_prolog(Term) :-
    format('Content-type: text/x-prolog~n~n'),
    write_term(Term,
	       [ quoted(true),
		 ignore_ops(true),
		 fullstop(true),
		 nl(true)
	       ]).


to_json(create(ID, Features)) :-
    memberchk(slave_limit=Max, Features),
    (   memberchk(answer=EventTerm, Features)
    ->  event_term_to_json_data(EventTerm, EventTermJson),
        Data = json([slave_limit=Max, answer=EventTermJson])
    ;   Data = json([slave_limit=Max])
    ),
    reply_json(json([event=create, id=ID, data=Data])).
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
to_json(debug(ID, Term)) :-
    reply_json(json([event=debug, id=ID, data=Term])).
to_json(abort(ID)) :-
    reply_json(json([event=abort, id=ID])).
to_json(destroy(ID)) :-
    reply_json(json([event=destroy, id=ID])).
to_json(destroy(ID, Data)) :-
    event_term_to_json_data(Data, JSON),
    reply_json(json([event=destroy, id=ID, data=JSON])).



to_json_s(create(ID, Features)) :-
    memberchk(slave_limit=Max, Features),
    (   memberchk(answer=EventTerm, Features)
    ->  event_term_to_json_s_data(EventTerm, EventTermJson),
        Data = json([slave_limit=Max, answer=EventTermJson])
    ;   Data = json([slave_limit=Max])
    ),
    reply_json(json([event=create, id=ID, data=Data])).
to_json_s(stop(ID)) :-
    reply_json(json([event=stop, id=ID])).
to_json_s(success(ID, Bindings0, More)) :-
    maplist(solution_to_json, Bindings0, Bindings),
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
to_json_s(debug(ID, Term)) :-
    reply_json(json([event=debug, id=ID, data=Term])).
to_json_s(abort(ID)) :-
    reply_json(json([event=abort, id=ID])).
to_json_s(destroy(ID)) :-
    reply_json(json([event=destroy, id=ID])).
to_json_s(destroy(ID, Data)) :-
    event_term_to_json_s_data(Data, JSON),
    reply_json(json([event=destroy, id=ID, data=JSON])).


event_term_to_json_data(success(ID, Bindings0, More),
        json([event=success, id=ID, data=Bindings, more= @(More)])) :- !,
    term_to_json(Bindings0, Bindings).
event_term_to_json_data(EventTerm, json([event=F, id=ID])) :-
    functor(EventTerm, F, 1), !,
    arg(1, EventTerm, ID).
event_term_to_json_data(EventTerm, json([event=F, id=ID, data=JSON])) :-
    functor(EventTerm, F, 2),
    arg(1, EventTerm, ID),
    arg(2, EventTerm, Data),
    term_to_json(Data, JSON).


event_term_to_json_s_data(success(ID, Bindings0, More),
        json([event=success, id=ID, data=Bindings, more= @(More)])) :- !,
    maplist(solution_to_json, Bindings0, Bindings).
event_term_to_json_s_data(EventTerm, json([event=F, id=ID])) :-
    functor(EventTerm, F, 1), !,
    arg(1, EventTerm, ID).
event_term_to_json_s_data(EventTerm, json([event=F, id=ID, data=JSON])) :-
    functor(EventTerm, F, 2),
    arg(1, EventTerm, ID),
    arg(2, EventTerm, Data),
    term_to_json(Data, JSON).


solution_to_json(BindingsIn, json(BindingsOut)) :-
    maplist(swap, BindingsIn, BindingsOut).

swap(N=V, N=A) :- term_to_atom(V, A).


%%	allowed(+Request) is det.
%
%	Check whether the peer is allowed to connect.  Returns a
%	=forbidden= header if contact is not allowed.

allowed(Request, Application) :-
	setting(Application:allow_from, Allow),
	match_peer(Request, Allow),
	setting(Application:deny_from, Deny),
	\+ match_peer(Request, Deny), !.
allowed(Request, _Application) :-
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

pengine_src_list(Application, ClauseList) :-
    maplist(expand_and_assert(Application), ClauseList).


/** pengine_src_text(+SrcText) is det

Asserts the clauses defined in SrcText   in the private dynamic database
of  the  current  Pengine.   See   also    the   `src_text'   option  of
pengine_create/1.

*/

pengine_src_text(Application, Src) :-
    setup_call_cleanup(
	open_chars_stream(Src, Stream),
	read_source(Application, Stream),
	close(Stream)).


/** pengine_src_url(+URL) is det

Asserts the clauses defined in URL in   the  private dynamic database of
the current Pengine. See also the `src_url' option of pengine_create/1.
*/

pengine_src_url(Application, URL) :-
    setup_call_cleanup(
	http_open(URL, Stream, []),
	read_source(Application, Stream),
	close(Stream)).

read_source(Application, Stream) :-
    read(Stream, Term),
    read_source(Application, Term, Stream).

read_source(_Application, end_of_file, _Stream) :- !.
read_source(Application, Term, Stream) :-
    expand_and_assert(Application, Term),
    read_source(Application, Stream).


expand_and_assert(Application, Term) :-
    expand_term(Term, ExpandedTerm),
    (   is_list(ExpandedTerm)
    ->  maplist(assert_local(Application), ExpandedTerm)
    ;   assert_local(Application, ExpandedTerm)
    ).


assert_local(Application, :-(Head, Body)) :- !,
    functor(Head, F, N),
    thread_local(Application:(F/N)),
    assert(Application:(Head :- Body)).
assert_local(Application, :-Body) :- !,
    (   safe_goal(Application:Body)
    ->  call(Application:Body)
    ;   true
    ).
assert_local(Application, Fact) :-
    functor(Fact, F, N),
    thread_local(Application:(F/N)),
    assert(Application:Fact).


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


		 /*******************************
		 *	  SANDBOX SUPPORT	*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1,		% Goal
	sandbox:safe_meta/2.			% Goal, Calls

%%	sandbox:safe_primitive(+Goal) is semidet.
%
%	Declare the core pengine operations as   safe. If we are talking
%	about  local  pengines,  their  safety   is  guaranteed  by  the
%	sandboxing done for all pengines.
%
%	@tbd	If at some point we allow for `unsafe' pengines, we must
%		reconsider this.

sandbox:safe_primitive(pengine:pengine_create(_)).
sandbox:safe_primitive(pengine:pengine_event(_, _)).
sandbox:safe_primitive(pengine:pengine_send(_, _, _)).
sandbox:safe_primitive(pengine:pengine_input(_, _)).
sandbox:safe_primitive(pengine:pengine_output(_, _)).
sandbox:safe_primitive(pengine:pengine_debug(_,_)).

sandbox:safe_primitive(pengine:pengine_rpc(_, _, _)).


sandbox:safe_meta(pengine:pengine_event_loop(_,Closure,_,_), [Closure1]) :-
	extend_goal(Closure, [_], Closure1).

extend_goal(Var, _, _) :-
	var(Var), !,
	instantiation_error(Var).
extend_goal(M:Term0, Extra, M:Term) :-
	extend_goal(Term0, Extra, Term).
extend_goal(Atom, Extra, Goal) :-
	atom(Atom), !,
	Goal =.. [Atom|Extra].
extend_goal(Compound, Extra, Goal) :-
	compound(Compound), !,
	compound_name_arguments(Compound, Name, Args0),
	append(Args0, Extra, Args),
	compound_name_arguments(Goal, Name, Args).
