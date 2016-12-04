% Copyright 2016 The CoALP Authors.

% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program. If not, see <http://www.gnu.org/licenses/>.

:- dynamic clauseenum/3.
:- dynamic clauselink/2.
:- dynamic cached_subterms/3.

load(File):-
    load(File,[]).
    
load(File, Options):-
    handle_options(Options),
    load_program(File,Refs),
    forall(member(Ref,Refs),clause_enum(Ref)),
    forall(member(Ref,Refs),cache_subterms(Ref)),
    if_option(checkcycles,
              (forall(member(Ref,Refs),assert_links_ref(Ref)),
               flood_links)),
    if_option(gcheck,ignore(catch(gcheck(Refs)))).

cache_subterms(Ref):-
    clause(_:Head,_,Ref),
    subterms(Head,Subterms),
    assert(cached_subterms(Ref,Head,Subterms)).

flood_links:-
    repeat,
    forall((clauselink(X,Y),clauselink(Y,Z),not(clauselink(X,Z))),
           (assert(clauselink(X,Z)),asserted=true)),
    not(asserted=true),!.

assert_links_ref(Ref):-
    clause(_:Head,Body,Ref),
    assert_links(Head,Body).
    
assert_links(_Head,true):-!.

assert_links(Head,(Goal,Goals)):-!,
    assert_links(Head,Goal),
    assert_links(Head,Goals).

assert_links(Head,(Cond -> Then ; Else)):-!,
    assert_links(Head,Cond),
    assert_links(Head,Then),
    assert_links(Head,Else).

assert_links(Head,(Goal1 ; Goal2)):-!,
    assert_links(Head,Goal1),
    assert_links(Head,Goal2).

assert_links(Head,once(Goal)):-!,
    assert_links(Head,Goal).

assert_links(_Head,Goal):-
    functor(Goal,Pred,Arity),
    builtin(Pred/Arity),!.

assert_links(Head,Goal):-
    assert_link(Head,Goal).

assert_link(Head,Goal):-
    functor(Head,HName,HArity),
    functor(Goal,GName,GArity),
    (clauselink(HName/HArity,GName/GArity)
     ;
     assert(clauselink(HName/HArity,GName/GArity))),!.

clause_next_enum(Pred/Arity,Next):-
    aggregate(max(Num),clauseenum(_,Pred/Arity,Num),Max),!,
    plus(Max,1,Next).

clause_next_enum(_Pred/_Arity,0).

clause_enum(Ref):-
    clause(_:Head,_,Ref),
    functor(Head,Pred,Arity),
    clause_next_enum(Pred/Arity,Next),
    assert(clauseenum(Ref,Pred/Arity,Next)).

load_program(File,Refs):-
    open(File,read,Stream),
    (import_module(program,user)->delete_import_module(program,user);true),
    forall(current_predicate(program:P),abolish(program:P)),
    retractall(coinductive(_)),
    retractall(clauseenum(_)),
    retractall(clauselink(_)),
    retractall(cached_subterms(_)),
    call_cleanup(assert_stream(Stream,Refs),close(Stream)).
               
assert_stream(Stream,[]) :-
    at_end_of_stream(Stream),!.

assert_stream(Stream,Refs1) :-
    read_term(Stream,Clause,[]),
    assert_clause(Clause,Refs1,Refs2),
    assert_stream(Stream,Refs2).


assert_clause((:-op(Precedence,Type,Name)),Refs,Refs):-!,
    program:op(Precedence,Type,Name).

assert_clause((:-coinductive(X)),Refs,Refs):-!,
    assert(coinductive(X)).

assert_clause((:-Directive),Refs,Refs):-!,
    format('ignoring unknown directive: :-~w.\n',[Directive]).

assert_clause(Clause,[Ref|Refs],Refs):-
    assert(program:Clause,Ref).