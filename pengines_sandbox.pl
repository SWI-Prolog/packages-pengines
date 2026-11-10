/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(pengines_sandbox, []).
:- use_module(library(pengines), []).
:- autoload(library(error),
            [ instantiation_error/1, type_error/2, permission_error/3,
              must_be/2
            ]).
:- autoload(library(lists), [member/2, append/3]).

/** <module> Declare Pengine interaction sandbox-safe

This module can be  loaded  alongside   library(pengines)  to  allow for
calling remote pengines from a sandboxed environment. This is disallowed
by default because one of the  use-cases   of  sandboxed  pengines is to
provide a generic application interface. In such   cases you do not want
the application to act as a proxy, in particular not to other systems in
a protected network.

In addition, sandboxed pengines may only access remote pengines (or load
source using the `src_url` option) if  the  setting `allow_remote` of
the pengine application is `true` (default  `false`). The hook
pengines:allow_remote_server/2 can be used to restrict the servers that
may be accessed. Sandboxed pengines can only address themselves and the
pengines they created.  Options that are passed to http_open/3 are
limited to options that do not access local resources such as files.
*/

                 /*******************************
                 *    PENGINE SANDBOX SUPPORT   *
                 *******************************/

:- multifile
    sandbox:safe_primitive/1,               % Goal
    sandbox:safe_meta/2.                    % Goal, Calls

%!  sandbox:safe_primitive(+Goal) is semidet.
%
%   Declare the core pengine operations as   safe. If we are talking
%   about  local  pengines,  their  safety   is  guaranteed  by  the
%   sandboxing done for all pengines.
%
%   @tbd    If at some point we allow for `unsafe' pengines, we must
%           reconsider this.

sandbox:safe_primitive(pengines:pengine_event(_, _)).
sandbox:safe_primitive(pengines:pengine_send(_, _, Options)) :-
    safe_options(Options, Calls),       % pengine_send/3 is not exported
    (   Calls == []
    ->  true
    ;   permission_error(use, option, Options)
    ).
sandbox:safe_primitive(pengines:pengine_user(_)).
sandbox:safe_primitive(system:'#file'(_,_)).

%!  sandbox:safe_meta(+Goal, -Called) is semidet.
%
%   Declare the pengine  meta-predicates  as   safe.  Note  that the
%   pengine calling predicates  are  safe   because  the  safety  is
%   guaranteed by the recieving pengine.

sandbox:safe_meta(pengines:pengine_create(Options), Calls) :-
    safe_options(Options, Calls).
sandbox:safe_meta(pengines:pengine_rpc(_, _, Options), Calls) :-
    safe_options(Options, Calls).
sandbox:safe_meta(pengines:pengine_ask(_, _, Options), Calls) :-
    safe_options(Options, Calls).
sandbox:safe_meta(pengines:pengine_destroy(_, Options), Calls) :-
    safe_options(Options, Calls).
sandbox:safe_meta(pengines:pengine_pull_response(_, Options), Calls) :-
    safe_options(Options, Calls).

%!  safe_options(+Options, -Calls) is det.
%
%   Verify the options for creating and interacting with pengines.  For
%   remote pengines, these options are passed to http_open/3 and thus
%   to ssl_context/3.  Only allow for options that do not access local
%   resources.  Hooks are verified as meta-calls.

safe_options(Options, _) :-
    var(Options),
    !,
    instantiation_error(Options).
safe_options([], []) :-
    !.
safe_options([H|T], Calls) :-
    !,
    safe_option(H, Calls, Calls1),
    safe_options(T, Calls1).
safe_options(Options, _) :-
    type_error(list, Options).

safe_option(Opt, _, _) :-
    var(Opt),
    !,
    instantiation_error(Opt).
safe_option(src_predicates(PIs), Calls, Calls) :-
    !,
    (   ground(PIs)
    ->  (   sub_term(Sub, PIs),
            compound(Sub),
            Sub = _:_
        ->  permission_error(list, qualified_predicate, PIs)
        ;   true
        )
    ;   instantiation_error(PIs)
    ).
safe_option(cacerts(List), Calls, Calls) :-
    !,
    must_be(list, List),
    (   member(E, List),
        \+ safe_cacert(E)
    ->  permission_error(use, option, cacerts(List))
    ;   true
    ).
safe_option(Opt, [Call|Calls], Calls) :-
    hook_option(Opt, Goal, Arity),
    !,
    hook_call(Goal, Arity, Call).
safe_option(Opt, Calls, Calls) :-
    compound(Opt),
    compound_name_arity(Opt, Name, 1),
    safe_option_name(Name),
    !.
safe_option(Opt, _, _) :-
    permission_error(use, option, Opt).

safe_cacert(E) :-
    var(E),
    !,
    instantiation_error(E).
safe_cacert(system(root_certificates)).
safe_cacert(certificate(_)).

% Pengine options
safe_option_name(application).
safe_option_name(ask).
safe_option_name(template).
safe_option_name(chunk).
safe_option_name(bindings).
safe_option_name(breakpoints).
safe_option_name(destroy).
safe_option_name(id).
safe_option_name(alias).
safe_option_name(src_text).
safe_option_name(src_list).
safe_option_name(src_url).
safe_option_name(server).
safe_option_name(listen).
safe_option_name(delay).
safe_option_name(force).
safe_option_name(timeout).
% http_open/3 options
safe_option_name(connection_timeout).
safe_option_name(user_agent).
safe_option_name(authorization).
safe_option_name(request_header).
safe_option_name(proxy).
safe_option_name(proxy_authorization).
% ssl_context/3 options that do not access files
safe_option_name(certificate_key_pairs).
safe_option_name(password).
safe_option_name(require_crl).
safe_option_name(cipher_list).
safe_option_name(ecdh_curve).
safe_option_name(min_protocol_version).
safe_option_name(max_protocol_version).
safe_option_name(disable_ssl_methods).

%!  hook_option(+Option, -Goal, -ExtraArity) is semidet.

hook_option(cert_verify_hook(Goal),  Goal, 5).
hook_option(pem_password_hook(Goal), Goal, 2).

%!  hook_call(+Goal, +Arity, -Call) is det.
%
%   SSL hooks that are not module qualified are called in the module
%   http_ssl_plugin.

hook_call(Goal, _, _) :-
    var(Goal),
    !,
    instantiation_error(Goal).
hook_call(Goal, Arity, Call) :-
    length(Extra, Arity),
    (   Goal = _:_
    ->  extend_goal(Goal, Extra, Call)
    ;   extend_goal(http_ssl_plugin:Goal, Extra, Call0),
        Call0 = M:Plain,
        (   predicate_property(M:Plain, imported_from(M2))
        ->  Call = M2:Plain
        ;   Call = Call0
        )
    ).
sandbox:safe_meta(pengines:pengine_event_loop(_,Closure,_,_), [Closure1]) :-
    extend_goal(Closure, [_], Closure1).

extend_goal(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
extend_goal(M:Term0, Extra, M:Term) :-
    !,
    extend_goal(Term0, Extra, Term).
extend_goal(Atom, Extra, Goal) :-
    atom(Atom),
    !,
    Goal =.. [Atom|Extra].
extend_goal(Compound, Extra, Goal) :-
    compound(Compound),
    !,
    compound_name_arguments(Compound, Name, Args0),
    append(Args0, Extra, Args),
    compound_name_arguments(Goal, Name, Args).
