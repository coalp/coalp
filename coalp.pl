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

:- include(init).

:- op(1000,fx,coinductive).

:- dynamic coninductive/1. 

default(gcheck).
default(checkcycles).

list:-
    add_import_module(program,user,start),
    program:listing,
    delete_import_module(program,user).

query(Goal):-
    query(Goal,[]).

query(Goal,Options):-
    handle_options(Options),
    copy_term(Goal,Root),
    catch(prove_(Goal,[],[],C)),
    format('Goal: ~w\n',[Goal]),
    print_coind(Root,C).

print_coind(_,[]).

print_coind(Root,[gc(Goal,_OGoal,Gctx)|Gc]):-
    functor(Goal,Pred,Arity),
    format('Hypothesis: ~w to ~w with guarding context ~w for predicate ~w/~w \n',[Root,Goal,Gctx,Pred,Arity]),
    print_coind(Root,Gc).

% needed for clauses with empty body (Head:-true)
prove_(true,_,C,C):-!.

% Goal , Goal
prove_((Goal,Goals),Trail,C1,C3):-
    !,
    prove_(Goal,Trail,C1,C2),
    prove_(Goals,Trail,C2,C3).

% Goal -> Goal ; Goal
prove_((Cond -> Then ; Else),Trail,C1,C2):-
    ( prove_(Cond,Trail,C1,C3) -> 
        prove_(Then,Trail,C3,C2)
        ; 
        prove_(Else,Trail,C1,C2)
    ).

% Goal ; Goal
prove_((Goal1 ; Goal2),Trail,C1,C2):-
    prove_(Goal1,Trail,C1,C2)
    ; 
    prove_(Goal2,Trail,C1,C2).

% once(Goal)
prove_(once(Goal),Trail,C1,C2):-
    prove_(Goal,Trail,C1,C2),!.

% builtin goal
prove_(Goal,_,C1,C1):-
    functor(Goal,Pred,Arity),
    ifnot_option(allbuiltin,builtin(Pred/Arity)),
    current_predicate(Pred/Arity),
    !,
    call(Goal),
    acyclic_term(Goal).

% coinductive goal
prove_(Goal,Trail,C1,C2):-
    functor(Goal,Pred,Arity),
    coinductive(Pred/Arity),!,
    copy_term(Goal,OGoal),
    clause(program:Goal,Body,Ref),
    acyclic_term(Goal),
    clause(program:Head,_,Ref),
    if_option(loopcheck,loopcheck([t(Ref,OGoal,Goal,[])|Trail])),
    (subsumes_term(Head,OGoal)->
        prove_(Body,[t(Ref,OGoal,Goal,[])|Trail],C1,C2)
        ;
        (
            cached_subterms(Ref,Head,Subterms),
            varcms(Goal,OGoal,Reducts),
            projs(Subterms,Reducts,Projs),
            goalsbyref(Ref,Trail,Goals),
            gcontext(Projs,Goals,Gctx),
            copy_term(Gctx,Gctx_copy),
            (fixpoint(Ref,Gctx_copy,Trail) ->
                C2=[gc(Goal,OGoal,Gctx_copy)|C1]
                ;
                prove_(Body,[t(Ref,OGoal,Goal,Gctx_copy)|Trail],C1,C2)
            )
        )
    ).

% inductive goal
prove_(Goal,Trail,C1,C2):-
    clause(program:Goal,Body,Ref),
    acyclic_term(Goal),
    copy_term(Goal,OGoal),
    if_option(loopcheck,loopcheck([t(Ref,OGoal,Goal,[])|Trail])),
    prove_(Body,[t(Ref,OGoal,Goal,[])|Trail],C1,C2).


% checks if Gctx1 is exists in Trail from the same clause (Ref)
fixpoint(Ref,Gctx1,Trail):-
    Gctx1=[_|_],
    msort(Gctx1,Gctx1_sorted),
    member(t(Ref,_,_,Gctx2),Trail),
    Gctx2=[_|_],
    msort(Gctx2,Gctx2_sorted),
    maplist('=@=',Gctx1_sorted,Gctx2_sorted).

% gathers all projections that guard a loop
gcontext(Projs,Goals,Gctx):-
    include(gcontextcheck(Goals),Projs,Gctx).

gcontextcheck(Goals,Proj):-
    append(_,[G1|Gs],Goals),
    append(_,[G2|_],Gs),
    reccms(G2,G1,CM),
    %format("~w to ~w found reccms ~w\n",[G2,G1,CM]),nl,
    matches_any(Proj,CM).

% gathers goals from the same clause (Ref)
goalsbyref(_,[],[]):-!.

goalsbyref(Ref,[t(Ref,_,G,_)|T],[G|Gs]):-
    !,
    goalsbyref(Ref,T,Gs).    
    
goalsbyref(Ref,[_|T],Gs):-
    goalsbyref(Ref,T,Gs). 
    
% computes projections from subterms and reducts
projs(Subterms,Reducts,Proj):-
    projs(Subterms,Reducts,[],Proj).

projs([],_,Proj,Proj).

projs([Subterm|Subterms],Reducts,Proj1,[Subterm|Proj2]):-
    matches_any(Subterm,Reducts),!,
    projs(Subterms,Reducts,Proj1,Proj2).

projs([Subterm|Subterms],Reducts,Proj1,[Subterm|Proj2]):-
    projs(Subterms,Reducts,Proj1,Proj2).

matches_any((S,_),[(T,_)|_]):-
    subsumes_term(S,T),!.

matches_any(C,[_|Ts]):-
    matches_any(C,Ts).

% gathers all reccursive contraction measures
reccms(F,T,CMs):-
    findall(CM,reccm(F,T,[],CM),CMs).
    
reccm(F,T,Path,(F,Path)):-
    compound(F),
    (var(T)->true;functor(T,_,0)),
    hassubterm(F,T).

reccm(F,T,Path,CM):-
    compound(F),
    compound(T),
    functor(F,Name,Arity),
    functor(T,Name,Arity),
    arg0(Pos,F,FArg),
    arg0(Pos,T,TArg),
    reccm(FArg,TArg,[Pos|Path],CM).

% gathers all variable contraction measures
varcms(F,T,CMs):-
    findall(CM,varcm(F,T,[],CM),CMs).

varcm(F,T,Path,(F,Path)):-
    compound(F),
    var(T).

varcm(F,T,Path,CM):-
    compound(F),
    compound(T),
    functor(F,Name,Arity),
    functor(T,Name,Arity),
    arg0(Pos,F,FArg),
    arg0(Pos,T,TArg),
    varcm(FArg,TArg,[Pos|Path],CM).    

hassubterm(T,S):-
    T==S.

hassubterm(T,S):-
    compound(T),
    arg(_,T,Arg),
    hassubterm(Arg,S).

subterms(Term,Subterms):-
    compound(Term),!,
    findall(Subterm,(arg0(Pos,Term,Arg),subterm(Arg,[Pos],Subterm)),Subterms).

subterms(_,[]).

subterm(Term,Path,(Term,Path)):-
    nonvar(Term).

subterm(Term,Path,Subterm):-
    compound(Term),
    arg0(Pos,Term,Arg),
    subterm(Arg,[Pos|Path],Subterm).

% 0 index based version of arg/3
arg0(Pos0,Term,Arg):-
    arg(Pos,Term,Arg),
    plus(Pos0,1,Pos).

% checks for an unguarded loop in a trail list
loopcheck([]):-!.
loopcheck([_]):-!.
loopcheck([T|Ts]):-
    T=t(Ref,_,Goal,_),
    loopcheck_(Ref,Goal,Ts,[T|Ts]).
    
loopcheck_(_,_,[],_).
loopcheck_(Ref,Goal,[t(Ref,_,TGoal,_)|Ts],Trail):-!,
    (reccms(TGoal,Goal,[])->
     throw(error(loop(Trail)))
     ;
     loopcheck_(Ref,Goal,Ts,Trail)
    ).

loopcheck_(Ref,Goal,[_|Ts],Trail):-
    loopcheck_(Ref,Goal,Ts,Trail).
