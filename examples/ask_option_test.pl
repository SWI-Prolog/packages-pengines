:- use_module(library(pengines)).

%:- debug(pengine(_)).


run :- 
    pengine_create([
        ask(member(A, [a,b,c,d,e])),
        template(A), 
        chunk(2),
        destroy(true)
    ]),
    pengine_event_loop(handle, []).


handle(create(ID, ProbeResult)) :-
    writeln(create(ID, ProbeResult)).
handle(success(ID, A, false)) :-
    writeln(A).
handle(success(ID, A, true)) :-
    writeln(A),
    pengine_next(ID, []).
handle(error(_ID, Err)) :-
    message_to_string(Err, ErrS),
    writeln(ErrS).


