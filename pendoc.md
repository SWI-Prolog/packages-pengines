# An overview of Pengines		{#pengine-overview}

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
      other processes may assert clauses.  These clauses reside in the
      module =pengine_sandbox=.

    * A message queue for incoming requests

    * A message queue for outgoing responses

Everything needed to work with  pengines   is  included  in the package,
including  a  JavaScript  library  for  creating  and  interacting  with
pengines from a web  client.  However,  the   web  server  (in  the file
examples/server.pl) should only be regarded as a minimal example.

Underlying the design of the  package  is   a  careful  analysis  of the
conversations taking place between Prolog and a   user (which could be a
human or another  piece  of  software).   Such  conversations  follow  a
communication protocol that we refer to as the Prolog Transport Protocol
(PLTP).  The  protocol  has  been  modelled    by  means  of  so  called
_communicating finite-state machines_ [3]. A  slight modification of the
protocol -- referred to as PLTP(HTTP) --   enables  us to synchronize it
with HTTP. The diagram  below   depicts  the  communicating finite-state
machines for PLTP(HTTP) and HTTP. Labels  in bold indicate requests, and
labels with a slash in front indicate responses.

[[pltpsynch.png]]

The diagram below depicts a PLTP run (on the right) corresponding to a
user's interaction with Prolog (on the left). `1234' is the Pengine's
identifier, which is a UUID in the actual implementation.

[[pltpruncolour.png]]

As for the relations between pengines, and   for the time being, we have
opted for a simple _master-slave   architecture_.  Once the master/slave
relationships are established, the direction of   control is always from
the master to the slaves. One or  more pengines can be _orchestrated_ by
a common master which can be an ordinary Prolog thread, another pengine,
or a JavaScript process. A slave  is   always  a pengine, running either
locally or remotely with respect to its   master.  Subject to a setting,
slaves are also dependent on their masters in the sense that if a master
terminates, so do its slaves.

[[penarch.png]]

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
[4]. Adding Pengines functionality to the Cliopatria platform [5] is
 straightforward.

A note about safety: Because PLTP is  layered   on  top  of HTTP, it may
utilize any standard HTTP security feature,  such as HTTP authentication
or SSL. Moreover, subject to  a   setting,  the library uses safe_goal/1
[6], which determines whether it is safe for   a slave pengine to try to
solve queries asked by a master.


## Pengine references		{#pengine-references}

 1. http://www.swi-prolog.org/pldoc/man?section=threads

 2. http://www.swi-prolog.org/pldoc/package/http.html

 3. D. Brand and P. Zafiropulo. On communicating finite-state machines.
   _Journal of the ACM_, 30(2):323-342, 1983.

 4. http://www.swi-prolog.org/pldoc/package/semweb.html

 5. http://cliopatria.swi-prolog.org/home

 6. http://www.swi-prolog.org/pldoc/doc/home/vnc/prolog/lib/swipl/library/sandbox.pl



## Pengine by examples		{#pengine-examples}

In this example we load the pengines library and use pengine_create/1 to
create a slave pengine in a remote   pengine server, and inject a number
of clauses in it. We then  use   pengine_event_loop/2  to start an event
loop that listens for three kinds of  event terms. Running =main/0= will
write  the  terms  q(a),  q(b)  and   q(c)  to  standard  output.  Using
pengine_ask/3 with the option template(X) would instead produce the output
=a=, =b= and =c=.

==
:- use_module(library(pengines)).

main :-
    pengine_create([
        server('http://pengines.swi-prolog.org'),
        src_text("
            q(X) :- p(X).
            p(a). p(b). p(c).
        ")
    ]),
    pengine_event_loop(handle, []).


handle(create(ID, _)) :-
    pengine_ask(ID, q(_X), []).
handle(success(_ID, [X], false)) :-
    writeln(X).
handle(success(ID, [X], true)) :-
    writeln(X),
    pengine_next(ID, []).
==

Here is another example, showing  how  to   create  and  interact with a
pengine from JavaScript in a way that seems ideal for Prolog programmers
and JavaScript programmers alike.   Loading the page
brings up the browser's prompt dialog, waits   for the user's input, and
writes that input in the browser  window.   If  the input was 'stop', it
stops there, else it repeats. Note that   I/O  works as expected. All we
need  to  do  is  to   use    pengine_input/1   instead  of  read/1  and
pengine_output/1 instead of write/1.

==
<html lang="en">
    <head>
        <script src="/vendor/jquery/jquery-2.0.3.min.js"></script>
        <script src="/pengine/pengines.js"></script>
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
?- use_module(library(pengines)).

?- member(X, [a, b, c, d]),
   pengine_rpc('http://pengines.org', p(X), [
       src_list([p(b), p(c), p(d), p(e)])
   ]),
   member(X, [c, d, e, f]).
X = c ;
X = d.
?-
==

## Making predicates available to clients {#pengine-server-code}

The code sent to a pengine is  executed   in  the  context of the module
=pengine_sandbox= and the safety of goals is validated using safe_goal/1
prior to execution. Any  pengine  has   access  to  the  safe predicates
defined in library(sandbox). If a server  wishes   to  extend the set of
predicates, it must:

  1. Define one or more modules that export the desired additional
     predicates.

  2. Makes this code available to the sandbox using the call below,
     assuming that the additional predicates are defined in the
     Prolog module file domain_predicates.pl

     ==
     :- use_module(pengine_sandbox:domain_predicates).
     ==

  3. Register *safe* foreign predicates with library(sandbox), i.e.,
     predicates that do not have side effects such as accessing the
     file system, load foreign extensions, define other predicates
     outside the sandbox environment, etc.

     Note that the safety of Prolog predicate can typically be
     proven by library(sandbox).  This may not be the case if
     untracktable forms of meta-calling are used.  In this case
     it is adviced to avoid such code.  If this is not possible,
     the code must be carefully reviewed by hand and of proven to
     be safe it may be registered with the sandbox library.

For example, basic RDF access can be  granted to pengines using the code
below.  Please  *study  the  sandboxing  code  carefully  before  adding
declarations*.

  ==
  :- use_module(pengine_sandbox:library(semweb/rdf_db)).
  :- use_module(library(sandbox)).

  :- multifile sandbox:safe_primitive/1.

  sandbox:safe_primitive(rdf_db:rdf(_,_,_)).
  ==


## Mapping Prolog terms into JSON	{#prolog-canonical-json}

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


## Pengine settings		{#pengine-settings}

Settings currently recognized by the Pengines library:


| *Name*		   | *Type*	| *Default* | *Description*						      |
| max_session_pengines	   | integer	| 1	    | Maximum number of pengines a client can create.  -1 is infinite |
| time_limit		   | number	| 60	    | Maximum time between output (in seconds)			      |
| allow_from		   | list(atom)	| [*]	    | Specify allowed IP addresses				      |
| deny_from		   | list(atom)	| []	    | Specify denied IP addresses.  Applied after =allow_from=.	      |

# Pengine libraries		{#pengine-libs}
