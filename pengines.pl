:- module(pengine,
	  [ pengine_create/1,
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
            pengine_seek_agreement/3,
            pengine_src_list/1,
            pengine_src_text/1,
            pengine_src_url/1,
            pengine_find_n/4
	  ]).

/** <module> Pengines: Web Logic Programming Made Easy

---++ Overview

This package provides a  powerful   high-level  programming  abstraction
implemented on top of SWI-Prolog's thread   predicates  [1] and its HTTP
client and server libraries [2]. The package makes it easy to create and
query _Prolog engines_ (or _Pengines_  for   short),  over HTTP, from an
ordinary Prolog thread, from a pengine, or  from JavaScript running in a
web  client.  Querying  follows   Prolog's  default  one-tuple-at-a-time
generation of solutions. I/O is also supported.

Possible  applications  abound,  but  in    particular  three  kinds  of
applications stick out: 1) The package provides   us with a useful point
of  departure  for  the  design  and  implementation  of  more  advanced
Prolog-based agent programming platforms, 2) it  suggests an elegant and
very straightforward approach to the building of a Semantic Web which is
Prolog-based in a very radical sense, and,   3)  it constitutes an ideal
way to interface Prolog with JavaScript,   the programming language most
commonly available in web browsers.

A pengine is comprised of:

    * A Prolog thread

    * A dynamic clause database, private to the pengine, into which
      other processes may assert clauses

    * A message queue for incoming requests

    * A message queue for outgoing responses

Everything needed to work with  pengines   are  included in the package,
including  a  JavaScript  library  for  creating  and  interacting  with
pengines from a web  client.  However,  the   web  server  (in  the file
example_server.pl) should only be regarded as a minimal example.

Underlying the design of the  package  is   a  careful  analysis  of the
conversations taking place between Prolog and a   user (which could be a
human or another  piece  of  software).   Such  conversations  follow  a
communication protocol that we refer to as the Prolog Transport Protocol
(PLTP).  The  protocol  has  been  modelled    by  means  of  so  called
_communicating finite-state machines_ [3]. A  slight modification of the
protocol -- referred to as PLTP(HTTP) --   enables  us to synchronize it
with HTTP. Figure 1 depicts the  communicating finite-state machines for
PLTP(HTTP) and HTTP. Labels in bold indicate requests, and labels with a
slash in front indicate responses.

![](lptp_synch.png)

As for the relations between pengines, and   for the time being, we have
opted for a simple _master-slave   architecture_.  Once the master/slave
relationships are established, the direction of   control is always from
the master to the slaves. One or  more pengines can be _orchestrated_ by
a common master which can be an ordinary Prolog thread, another pengine,
or a JavaScript process. A slave  is   always  a pengine, running either
locally or remotely with respect to its   master.  Subject to a setting,
slaves are also dependent on their masters in the sense that if a master
terminates, so do its slaves.

![](architecture.png)

The transport format is different depending on the nature of the master.
If the master is a JavaScript process,   it  will (by default) formulate
its requests using Prolog syntax,  and   get  responses  back as Prologs
terms encoded in JSON. If  the  master   is  a  Prolog process (a Prolog
thread or a pengine) it will (again  only by default) get responses back
as Prolog terms.

Most of the pengine predicates are   deterministic, yet they can control
one or more pengines solving possibly non-deterministic queries. But the
package also offers a number of   non-deterministic predicates, built on
top of the deterministic ones, that can  solve queries "the Prolog way",
binding query variables in the process, backtracking for more solutions.
Of these predicates, pengine_rpc/3 is the   most  important. By means of
pengine_rpc/3 a pengine running in a pengine   server A can call and try
to solve a query in the  context   of  another  pengine server B, taking
advantage of the data being offered by B,  just as if the data was local
to A. Thus, in theory, a Prolog program, be it a pure Horn clause theory
or not, can be as big as the Web.  This is something that should make us
think about a _Semantic Web_, especially  when we consider the excellent
fit between the Pengine library and   SWI-Prolog's  Semantic Web Library
[4]. Adding Pengines functionality to the Cliopatria platform [5] should
also be straightforward.

A note about safety: Because PLTP is  layered   on  top  of HTTP, it may
utilize any standard HTTP security feature,  such as HTTP authentication
or SSL. Moreover, subject to  a   setting,  the library uses safe_goal/1
[6], which determines whether it is safe for   a slave pengine to try to
solve queries asked by a master.


---++ References

 1. http://www.swi-prolog.org/pldoc/man?section=threads

 2. http://www.swi-prolog.org/pldoc/package/http.html

 3. D. Brand and P. Zafiropulo. On communicating finite-state machines.
   _Journal of the ACM_, 30(2):323-342, 1983.

 4. http://www.swi-prolog.org/pldoc/package/semweb.html

 5. http://cliopatria.swi-prolog.org/home

 6. http://www.swi-prolog.org/pldoc/doc/home/vnc/prolog/lib/swipl/library/sandbox.pl



---++ Examples

In this example we load the pengines library and use pengine_create/1 to
create a slave pengine in a remote   pengine server, and inject a number
of clauses in it. We then  use   pengine_event_loop/1  to start an event
loop that listens for three kinds of  event terms. Running =main/0= will
write  the  terms  q(a),  q(b)  and   q(c)  to  standard  output.  Using
pengine_ask/3 with the option template(X) instead of pengine_ask/2 would
instead produce the output =a=, =b= and =c=.

==
:- use_module(pengines).

main :-
    pengine_create([
        server('http://pengines.org'),
        src_text("
            q(X) :- p(X).
            p(a). p(b). p(c).
        ")
    ]),
    pengine_event_loop(handle).


handle(create(ID, _)) :-
    pengine_ask(ID, q(X)).
handle(success(ID, [X], false)) :-
    writeln(X).
handle(success(ID, [X], true)) :-
    writeln(X),
    pengine_next(ID).
==

Here is another example, showing  how  to   create  and  interact with a
pengine from JavaScript in a way that seems ideal for Prolog programmers
and JavaScript programmers/frontend developers alike.   Loading the page
brings up the browser's prompt dialog, waits   for the user's input, and
writes that input in the browser  window.   If  the input was 'stop', it
stops there, else it repeats. Note that   I/O  works as expected. All we
need  to  do  is  to   use    pengine_input/1   instead  of  read/1  and
pengine_output/1 instead of write/1.

==
<html lang="en">
    <head>
        <script src="/vendor/jquery/jquery-2.0.3.min.js"></script>
        <script src="/assets/js/pengine.js"></script>
        <script type="text/x-prolog">

            main :-
                repeat,
                pengine_input(X),
                pengine_output(X),
                X == stop.

        </script>
        <script>
            var pengine = new Pengine({
                oncreate: handleCreate,
                onprompt: handlePrompt,
                onoutput: handleOutput
            });
            function handleCreate() {
                pengine.ask('main');
            }
            function handlePrompt() {
                pengine.input(prompt(this.data));
            }
            function handleOutput() {
                $('#out').html(this.data);
            }
        </script>
    </head>
    <body>
        <div id="out"></div>
    </body>
</html>
==

Our third example shows that a non-deterministic predicate can be called
remotely by means of pengine_rpc/2,  yet   behave  exactly  as if called
locally:

==
?- use_module(pengines).

?- member(X, [a, b, c, d]),
   pengine_rpc('http://pengines.org', p(X), [
       src_list([p(b), p(c), p(d), p(e)])
   ]),
   member(X, [c, d, e, f]).
X = c ;
X = d.
?-
==

In our fourth and final  (and   admittedly  most complicated) example we
show how elegantly pengine_rpc/3, used in the previous example, can been
implemented using the Pengines  core   predicates  --  pengine_create/1,
pengine_event/2,            pengine_ask/3,             pengine_output/1,
pengine_pull_response/2 and pengine_next/2.

==

pengine_rpc(URL, Query, Options) :-
    pengine_create([
        server(URL)
        | Options
    ]),
    wait_event(Query, Options).


wait_event(Query, Options) :-
    pengine_event(Event),
    process_event(Event, Query, Options).


process_event(create(ID, _), Query, Options) :-
    pengine_ask(ID, Query, Options),
    wait_event(Query, Options).
process_event(error(_ID, Error), _Query, _Options) :-
    throw(Error).
process_event(failure(_ID), _Query, _Options) :-
    fail.
process_event(prompt(ID, Term), Query, Options) :-
    pengine_output(prompt(ID, Term)),
    wait_event(Query, Options).
process_event(output(ID, Term), Query, Options) :-
    pengine_output(output(ID, Term)),
    pengine_pull_response(ID, Options),
    wait_event(Query, Options).
process_event(success(_ID, Solutions, false), Query, _Options) :-
    member(Query, Solutions).
process_event(success(_ID, Solutions, true), Query, _Options) :-
    member(Query, Solutions).
process_event(success(ID, _Query, true), Query, Options) :-
    pengine_next(ID, Options),
    wait_event(Query, Options).

==

---++ Mapping Prolog terms into JSON

In Prolog, solutions to queries are given as bindings which map variable
names into Prolog terms. A  programmer   using  Pengines in a JavaScript
evironment needs to understand how bindings are converted into JSON. For
example, the programmer needs to understand  that the second solution to
=|append(Xs,   Ys,   [a,b,c])|=    is    given     by    the    bindings
=|['Xs'=[a],'Ys'=[b,c]]|= and that these binding   can be represented in
JSON as =|{"Xs":["a"], "Ys":["b","c"]}|=.

Pengines defines the following mapping between   ground Prolog terms and
JSON.

    * A Prolog atom is mapped to a JSON string.
    * A Prolog number is mapped to a JSON number.
    * A Prolog list is mapped to a JSON array.
    * The Prolog terms =|@(true)|= and =|@(false)|= are mapped to the
      JSON constants =true= and =false=, respectively.
    * The Prolog term =|@(null)|= is mapped to the JSON constant =null=.
    * A Prolog term json(NameValueList), where `NameValueList` is a
    list of `Name=Value` pairs, is mapped to a JSON object.

    * Any other complex Prolog term `T` is mapped to a JSON object of
    the form =|{"functor": F, "args": A}|= where `F` is a string
    representing the functor of `T` and `A` is the list of JSON values
    representing `T`s arguments.


---++ Settings

Settings currently recognized by the Pengines library:


| *Name*    | *Type* | *Default* | *Description* |
| allow_multiple_session_pengines    | atom | false | Allow a HTTP session to run more than one top level pengine at a time |
| time_limit    | number | 60 | Maximum time between output (in seconds) |
| pengine_alive_time_limit    | number | 360 | Maximum time to allow a pengine to live (in seconds) |
| report_protocol_breach | atom | false | Throw exception when protocol is breached |


---++ Predicates

*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(library(time)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(settings)).
:- use_module(library(debug)).
:- use_module(library(sandbox)).
:- use_module(library(term_to_json)).


:- meta_predicate
	pengine_event_loop(1),
	pengine_event_loop(1, +).

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

sandbox:safe_primitive(_:html(_, _, _)).
sandbox:safe_primitive(_:with_output_to(_, _)).

sandbox:safe_primitive(system:sleep(_)).
sandbox:safe_primitive(system:atom_concat(_, _, _)).



/* Settings */

:- setting(allow_multiple_session_pengines, atom, false,
	   'Allow a HTTP session to run more than one top \c
	   level pengine at a time').

:- setting(time_limit, number, 60, 'Maximum time between output').

:- setting(pengine_alive_time_limit, number, 60,
	   'Maximum time to allow a pengine to live').



/**  pengine_create(+Options) is det.

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


pengine_create(Options) :-
    (   select_option(server(BaseURL), Options, RestOptions)
    ->  remote_pengine_create(BaseURL, RestOptions)
    ;   local_pengine_create(Options)
    ).


/**  pengine_send(+NameOrID, +Term) is det

Same as pengine_send(NameOrID, Term, []).
*/

pengine_send(Target, Event) :-
    pengine_send(Target, Event, []).


:- thread_local current_alarm/1.

/**  pengine_send(+NameOrID, +Term, +Options) is det

Succeeds immediately and  places  Term  in   the  queue  of  the pengine
NameOrID. Options is a list of options:

   * delay(+Time)
     The actual sending is delayed by Time seconds. Time is an integer
     or a float.

Any remaining options are passed to http_open/3.
*/

pengine_send(Target0, Event0, Options) :-
    (   Target0 = self
    ->  thread_self(Target),
        Event = Event0
    ;   Target0 = parent
    ->  nb_getval(parent, Target),
        (   Event0 = output(_, _)
        ->  Event = Event0
        ;   thread_self(ID),
            Event = output(id(Target, ID), Event0)
        )
    ;   atom(Target0)
    ->  id(Target0, Target),
        Event = Event0
    ;   Target = Target0,
        Event = Event0
    ),
    (   option(delay(Delay), Options)
    ->  alarm(Delay, (
                send1(Target, Event, Options),
                retract(current_alarm(AlarmID))
            ), AlarmID, [remove(true)]),
        assert(current_alarm(AlarmID))
    ;   send1(Target, Event, Options)
    ).


send1(BaseURL:ID, Event, Options) :- !,
    remote_pengine_send(BaseURL, BaseURL:ID, Event, Options).
send1(id(_From, To), Event, _Options) :- !,
    thread_send_message(To, Event).
send1(RealID, Event, _Options) :-
    thread_send_message(RealID, Event).



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

pengine_abort(BaseURL:ID) :- !,
    remote_pengine_abort(BaseURL, BaseURL:ID, []).
pengine_abort(id(_F,T)) :- !,
    pengine_abort(T).
pengine_abort(ID) :-
    catch(thread_signal(ID, throw(abort_query)), _, true).


/** pengine_destroy(+NameOrID) is det

Destroys the pengine NameOrID.

@tbd	Should abort the pengine if it is running a query.
*/

pengine_destroy(ID) :-
    pengine_send(ID, request(destroy)).



/** pengine_property(+NameOrID, ?Property) is nondet.

True  when  Property  is  a  property  of  the  given  Pengine.  Defined
properties are properties of the associated   thread  and message queue,
with the following additional properties:

  * parent(Thread)
  Thread id for the parent (local) pengine.
  * self(Thread)
  Thread id of the running pengine.
*/

pengine_property(id(Thread, _), parent(Thread)).
pengine_property(id(_, Thread), self(Thread)).
pengine_property(id(_, Thread), Property) :-
    thread_property(Thread, Property).
pengine_property(id(Thread, _), Property) :-
    (   thread_property(Thread, Property)
    ;   message_queue_property(Thread, Property)
    ).


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

:- dynamic      child/2.		% Pengine, Child
:- thread_local id/2.			% Name, Child

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
    create(Self, Child, Options),
    assert(child(Self, Child)),
    (   option(name(Name), Options)
    ->  assert(id(Name, id(Self, Child)))
    ;   true
    ).

create(Self, Child, Options) :-
    select_option(probe(Condition), Options, RestOptions0, true),
    partition(pengine_create_option, RestOptions0, PengineOptions, RestOptions),
    (   catch(Condition, E, true)
    ->  (   var(E)
	->  thread_create(pengine_main(id(Self, Child), PengineOptions), Child,
			  [ at_exit(done(Self, Child))
			  | RestOptions
			  ]),
	    (	option(id(Id), Options)
	    ->	Id = id(Self, Child)
	    ;	true
	    )
	;   probe_failure(Self, E)
	)
    ;   probe_failure(Self, error(probe_failure(Condition), _))
    ).

probe_failure(Self, Term) :-
	thread_send_message(Self, error(id(null, null), Term)).


pengine_create_option(src_text(_)).
pengine_create_option(src_list(_)).
pengine_create_option(src_url(_)).
pengine_create_option(probe(_)).
pengine_create_option(probe_template(_)).

%%	done(+Parent, +Self)
%
%	Called  from  the  pengine  thread   =at_exit=  option.  Removes
%	possible pending alarms and  destroys   _child_  pengines  using
%	pengine_destroy/1.

:- public
	done/2.

done(_Parent, Self) :-
    forall(retract(current_alarm(AlarmId)),
	   catch(remove_alarm(AlarmId), _, true)),
    forall(retract(child(Self, Child)),
	   pengine_destroy(Child)).


%%	pengine_main(+Id, +Options)
%
%	Run a pengine main loop. First acknowledges its creation and run
%	pengine_main_loop/1.

pengine_main(ID, Options) :-
    parent(ID, Parent),
    nb_setval(parent, Parent),
    select_option(probe_template(Template), Options, RestOptions, true),
    (   call_options_as_goals(RestOptions, ID)
    ->  thread_send_message(Parent, create(ID, Template)),
        pengine_main_loop(ID)
    ;   true
    ).


call_options_as_goals([], _).
call_options_as_goals([Option|Options], ID) :-
    catch(Option, Error, true),
    (   var(Error)
    ->  call_options_as_goals(Options, ID)
    ;   send_error(ID, Error),
	fail
    ).

pengine_main_loop(ID) :-
    catch(guarded_main_loop(ID), abort_query,
	  ( parent(ID, Parent),
	    thread_send_message(Parent, abort(ID)),
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
    thread_get_message(Event),
    (   Event = request(destroy)
    ->  debug(pengine(transition), '~q: 2 = ~q => 1', [ID, destroy]),
        parent(ID, Parent),
        thread_send_message(Parent, destroy(ID)),
	thread_self(Me),		% Make the thread silently disappear
	thread_detach(Me)
    ;   Event = request(ask(Goal, Options))
    ->  debug(pengine(transition), '~q: 2 = ~q => 3', [ID, ask(Goal)]),
        ask(ID, Goal, Options)
    ;   debug(pengine(event), 'sending to ~q: protocol_error', [Parent]),
        parent(ID, Parent),
        %thread_send_message(Parent, error(ID, error(protocol_error, _))),
        guarded_main_loop(ID)
    ).


%%	solve(+Template, :Goal, +ID) is det.
%
%	Solve Goal. Note that because we can ask for a new goal in state
%	`6', we must provide for an ancesteral cut (prolog_cut_to/1). We
%	need to be sure to  have  a   choice  point  before  we can call
%	prolog_current_choice/1. This is the reason   why this predicate
%	has two clauses.

solve(Template, Goal, ID) :-
    parent(ID, Parent),
    prolog_current_choice(Choice),
    (   call_cleanup(catch(Goal, Error, true), Det=true),
        (   var(Error)
        ->  (   var(Det)
            ->  thread_send_message(Parent, success(ID, Template, true)),
                debug(pengine(event), 'sending to ~q: ~q',
		      [Parent, success(Template, true)]),
                more_solutions(ID, Choice)
            ;   !,			% commit
		thread_send_message(Parent, success(ID, Template, false)),
                debug(pengine(event), 'sending to ~q: ~q',
		      [Parent, success(Template, false)]),
                guarded_main_loop(ID)
            )
        ;   !,				% commit
	    thread_send_message(Parent, error(ID, Error)),
            debug(pengine(event), 'sending to ~q: ~q', [Parent, error(Error)]),
            guarded_main_loop(ID)
        )
    ;   !,				% commit
	thread_send_message(Parent, failure(ID)),
        debug(pengine(event), 'sending to ~q: failure', [Parent]),
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
    thread_get_message(Event),
    (   Event = request(stop)
    ->  debug(pengine(transition), '~q: 6 = ~q => 7', [ID, stop]),
        parent(ID, Parent),
        thread_send_message(Parent, stop(ID)),
        guarded_main_loop(ID)
    ;   Event = request(next)
    ->  debug(pengine(transition), '~q: 6 = ~q => 3', [ID, next]),
        fail
    ;   Event = request(ask(Goal, Options))
    ->  debug(pengine(transition), '~q: 2 = ~q => 3', [ID, ask(Goal)]),
	prolog_cut_to(Choice),
        ask(ID, Goal, Options)
    ;   debug(pengine(event), 'sending to ~q: protocol_error', [Parent]),
        parent(ID, Parent),
        %thread_send_message(Parent, error(ID, error(protocol_error, _))),
        more_solutions(ID, Choice)
    ).

%%	ask(+Pengine, :Goal, +Options)
%
%	Migrate from state `2' to `3'.  This predicate validates that it
%	is safe to call Goal using safe_goal/1 and then calls solve/3 to
%	prove the goal. It takes care of the paging(N) option.

ask(ID, Goal, Options) :-
    catch(safe_goal(Goal), Error, true),
    (   var(Error)
    ->  option(template(Template), Options, Goal),
        option(paging(N), Options, 1),
        (   N == 1
        ->  solve([Template], Goal, ID)
        ;   solve(Res, pengine_find_n(N, Template, Goal, Res), ID)
        )
    ;   parent(ID, Parent),
        thread_send_message(Parent, error(ID, Error))
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
    thread_self(Self),
    nb_getval(parent, Parent),
    pengine_get_prompt(Prompt),
    thread_send_message(Parent, prompt(id(Parent, Self), Prompt)),
    thread_get_message(input(Term)).



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


%%	send_error(+Pengine, +Error) is det.
%
%	Send an error to my parent.   Remove non-readable blobs from the
%	error term first using replace_blobs/2.

send_error(Pengine, Error) :-
	replace_blobs(Error, Error1),
	parent(Pengine, Parent),
	thread_send_message(Parent, error(Pengine, Error1)).

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
    partition(pengine_create_option, Options, PengineOptions, RestOptions),
    term_to_atom(PengineOptions, PengineOptionsAtom),
    uri_encoded(query_value, PengineOptionsAtom, PengineOptionsAtomEncoded),
    atomic_list_concat([BaseURL, '/pengine/create?options=', PengineOptionsAtomEncoded], URL),
    url_message(URL, Event, RestOptions),
    arg(1, Event, ID),
    ignore(option(id(ID), Options)),
    (   option(name(Name), Options)
    ->  assert(id(Name, ID))
    ;   true
    ),
    thread_self(Self),
    thread_send_message(Self, Event).


remote_pengine_send(BaseURL, ID, Event, Options) :-
    term_to_atom(ID, IdAtom),
    term_to_atom(Event, EventAtom),
    uri_encoded(query_value, IdAtom, IdAtomEncoded),
    uri_encoded(query_value, EventAtom, EventAtomEncoded),
    atomic_list_concat([BaseURL, '/pengine/send?id=', IdAtomEncoded, '&event=', EventAtomEncoded], URL),
    url_message(URL, Message, Options),
    thread_self(Self),
    thread_send_message(Self, Message).


remote_pengine_pull_response(BaseURL, ID, Options) :-
    term_to_atom(ID, IdAtom),
    uri_encoded(query_value, IdAtom, IdAtomEncoded),
    atomic_list_concat([BaseURL, '/pengine/pull_response?id=', IdAtomEncoded], URL),
    url_message(URL, Event, Options),
    thread_self(Self),
    thread_send_message(Self, Event).


remote_pengine_abort(BaseURL, ID, Options) :-
    term_to_atom(ID, IdAtom),
    uri_encoded(query_value, IdAtom, IdAtomEncoded),
    atomic_list_concat([BaseURL, '/pengine/abort?id=', IdAtomEncoded], URL),
    url_message(URL, Event, Options),
    thread_self(Self),
    thread_send_message(Self, Event).


url_message(URL, Message, Options) :-
    setup_call_cleanup(
	http_open(URL, Stream, Options),
	read(Stream, Message),
	close(Stream)).


/** pengine_event(?EventTerm) is det

Same as  pengine_event(EventTerm, []).

*/

pengine_event(Event) :-
    thread_get_message(Event).


/** pengine_event(?EventTerm, +Options) is det

Examines the pengine's event queue  and   if  necessary blocks execution
until a term that unifies to Term  arrives   in  the queue. After a term
from the queue has been unified to Term,   the  term is deleted from the
queue.

   Valid options are:

   * timeout(+Time)
     Time is a float or integer and specifies the maximum time to wait
     in seconds. If no event has arrived before the time is up EventTerm
     is bound to the atom `timeout'.

*/

pengine_event(Event, Options) :-
    thread_self(Self),
    (   thread_get_message(Self, Event, Options)
    ->  true
    ;   Event = timeout
    ).


/** pengine_event_loop(+Closure) is det

Same as pengine_event_loop(Closure, []).

*/

pengine_event_loop(Closure) :-
    pengine_event_loop(Closure, [], []).


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
     Forwards received event terms to slaves. To is either =all=,
     =all_but_sender= or a Prolog list of NameOrIDs. [not yet
     implemented]

*/

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

Semantically equivalent to Query, except that the   query is run in (and
in the Prolog context of) the pengine  server referred to by URL, rather
than locally.

Valid options are:

    * paging(+Integer)
      Can be used to reduce the number of network roundtrips being made.
      See pengine_ask/3.

    * guard(+Goal)
      The guard is run locally after each return of Query, and if it
      succeeds, the pengine behind the call is first stopped and then
      destroyed. If it fails, pengine_rpc/3 will backtrack and look for
      more solutions. [Not yet implemented]

Remaining  options  (except   the   server    option)   are   passed  to
pengine_create/1.
*/

pengine_rpc(URL, Query) :-
    pengine_rpc(URL, Query, []).

pengine_rpc(URL, Query, Options) :-
    setup_call_cleanup(
	pengine_create([ server(URL),
			 id(Id)
		       | Options
		       ]),
	wait_event(Query, Options),
	pengine_destroy(Id)).

wait_event(Query, Options) :-
    pengine_event(Event),
    process_event(Event, Query, Options).

process_event(create(ID, _), Query, Options) :-
    pengine_ask(ID, Query, Options),
    wait_event(Query, Options).
process_event(error(_ID, Error), _Query, _Options) :-
    throw(Error).
process_event(failure(_ID), _Query, _Options) :-
    fail.
process_event(prompt(ID, Term), Query, Options) :-
    pengine_output(prompt(ID, Term)),
    wait_event(Query, Options).
process_event(output(ID, Term), Query, Options) :-
    pengine_output(output(ID, Term)),
    pengine_pull_response(ID, Options),
    wait_event(Query, Options).
process_event(success(_ID, Solutions, _), Query, _Options) :-
    member(Query, Solutions).
process_event(success(ID, _Query, true), Query, Options) :-
    pengine_next(ID, Options),
    wait_event(Query, Options).




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
    http_parameters(Request,
		    [ options(OptionsAtom,
			      [ optional(true)
			      ]),
		      format(Format,
			     [ oneof([prolog, json, 'json-s']),
			       default(prolog)
			     ])
		    ]),
    (	var(OptionsAtom)
    ->	Options = []
    ;	atom_to_term(OptionsAtom, Options, _)
    ),
    request_to_url(Request, URL),
    (   setting(allow_multiple_session_pengines, false)
    ->  forall(http_session_retract(pengine(ID)),
	       pengine_destroy(ID))
    ;   true
    ),
    message_queue_create(From, []),
    create(From, To, Options),
    (   setting(allow_multiple_session_pengines, false)
    ->  http_session_assert(pengine(id(From, To)))
    ;   true
    ),
    wait_and_output_result(From, To, URL, Format).



request_to_url(Request, BaseURL) :-
    memberchk(protocol(Protocol), Request),
    memberchk(host(Host), Request),
    memberchk(port(Port), Request),
    atomic_list_concat([Protocol, '://', Host, ':', Port], BaseURL).



wait_and_output_result(Parent, To, URL, Format) :-
    setting(time_limit, TimeLimit),
    (   thread_get_message(Parent, Event, [timeout(TimeLimit)])
    ->  (   Event = done(Id)
        ->  thread_join(Id, _Message),
            ReturnEvent = destroy(Id)
        ;   ReturnEvent = Event
        ),
        output_result(Format, ReturnEvent, URL)
    ;   output_result(Format, error(id(Parent, To),
				    error(time_limit_exceeded, _)), URL),
        pengine_abort(To)
    ).



http_pengine_send(Request) :-
    http_parameters(Request,
            [   id(IdAtom, []),
                event(EventAtom, []),
                format(Format, [default(prolog)])
            ]),
    atom_to_term(IdAtom, ID, _),
    parent(ID, Parent),
    thread(ID, Thread),
    url(ID, URL),
    catch(
        (
            atom_to_term(EventAtom, Event0, Bindings),
            fix_bindings(Format, Event0, ID, Bindings, Event1)
        ),
        Error,
        true
    ),
    (   var(Error)
    ->  thread_send_message(Thread, Event1)
    ;   thread_send_message(Parent, error(ID, Error))
    ),
    wait_and_output_result(Parent, Thread, URL, Format).



url(URL:id(_, _), URL).

parent(id(Parent, _), Parent).
parent(_:id(Parent, _), Parent).

thread(id(_, Thread), Thread).
thread(_:id(_, Thread), Thread).



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
            [   id(IdAtom, []),
                format(Format, [default(prolog)])
            ]),
    atom_to_term(IdAtom, ID, _),
    parent(ID, Parent),
    thread(ID, Thread),
    request_to_url(Request, URL),
    wait_and_output_result(Parent, Thread, URL, Format).


http_pengine_abort(Request) :-
    http_parameters(Request,
            [   id(IdAtom, []),
                format(Format, [default(prolog)])
            ]),
    atom_to_term(IdAtom, ID, _),
    thread(ID, Thread),
    parent(ID, Parent),
    url(ID, URL),
    catch(thread_signal(Thread, throw(abort)), _, true),
    output_result(Format, abort(id(Parent, Thread)), URL).



% Output

output_result(prolog, Term0, BaseURL) :-
    add_URL_to_ID(Term0, BaseURL, Term),
    to_prolog(Term).
output_result(json, Term0, BaseURL) :-
    add_URL_to_ID(Term0, BaseURL, Term),
    to_json(Term).
output_result('json-s', Term0, BaseURL) :-
    add_URL_to_ID(Term0, BaseURL, Term),
    to_json_s(Term).


add_URL_to_ID(create(ID, Term), Base, create(Base:ID, Term)).
add_URL_to_ID(success(ID, Bindings, More), Base, success(Base:ID, Bindings, More)).
add_URL_to_ID(failure(ID), Base, failure(Base:ID)).
add_URL_to_ID(error(ID, Error), Base, error(Base:ID, Error)).
add_URL_to_ID(output(ID, Term), Base, output(Base:ID, Term)).
add_URL_to_ID(prompt(ID, Term), Base, prompt(Base:ID, Term)).
add_URL_to_ID(stop(ID), Base, stop(Base:ID)).
add_URL_to_ID(abort(ID), Base, abort(Base:ID)).
add_URL_to_ID(destroy(ID), Base, destroy(Base:ID)).



to_prolog(Term) :-
    format('Content-type: text/x-prolog~n~n'),
    format('~q.~n', [Term]).


to_json(create(ID0, Term0)) :-
    term_to_atom(ID0, ID),
    term_to_json(Term0, Term),
    reply_json(json([event=create, id=ID, data=Term])).
to_json(stop(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=stop, id=ID])).
to_json(success(ID0, Bindings0, More)) :-
    term_to_atom(ID0, ID),
    term_to_json(Bindings0, Bindings),
    reply_json(json([event=success, id=ID, data=Bindings, more= @(More)])).
to_json(failure(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=failure, id=ID])).
to_json(error(ID0, Error0)) :-
    term_to_atom(ID0, ID),
    message_to_string(Error0, Error),
    reply_json(json([event=error, id=ID, data=Error])).
to_json(output(ID0, Term0)) :-
    term_to_atom(ID0, ID),
    term_to_json(Term0, Json),
    reply_json(json([event=output, id=ID, data=Json])).
to_json(prompt(ID0, Term0)) :-
    term_to_atom(ID0, ID),
    term_to_json(Term0, Json),
    reply_json(json([event=prompt, id=ID, data=Json])).
to_json(abort(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=abort, id=ID])).
to_json(destroy(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=destroy, id=ID])).


to_json_s(create(ID0, Term0)) :-
    term_to_atom(ID0, ID),
    term_to_json(Term0, Term),
    reply_json(json([event=create, id=ID, data=Term])).
to_json_s(stop(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=stop, id=ID])).
to_json_s(success(ID0, Bindings0, More)) :-
    term_to_atom(ID0, ID),
    solution_to_json(Bindings0, Bindings),
    reply_json(json([event=success, id=ID, data=Bindings, more= @(More)])).
to_json_s(failure(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=failure, id=ID])).
to_json_s(error(ID0, Error0)) :-
    term_to_atom(ID0, ID),
    message_to_string(Error0, Error),
    reply_json(json([event=error, id=ID, data=Error])).
to_json_s(output(ID0, Term0)) :-
    term_to_atom(ID0, ID),
    term_to_json(Term0, Json),
    reply_json(json([event=output, id=ID, data=Json])).
to_json_s(prompt(ID0, Term0)) :-
    term_to_atom(ID0, ID),
    term_to_json(Term0, Json),
    reply_json(json([event=prompt, id=ID, data=Json])).
to_json_s(abort(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=abort, id=ID])).
to_json_s(destroy(ID0)) :-
    term_to_atom(ID0, ID),
    reply_json(json([event=destroy, id=ID])).


solution_to_json(BindingsIn, json(BindingsOut)) :-
    maplist(swap, BindingsIn, BindingsOut).

swap(N=V, N=A) :- term_to_atom(V, A).




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


% Short versions

src_list(ClauseList) :- pengine_src_list(ClauseList).

src_text(Src) :- pengine_src_text(Src).

src_url(URL) :- pengine_src_url(URL).


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
