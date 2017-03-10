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

% gcheck implements the guardedness check for clauses references
% in list Refs by a failure driven search space traversal
gcheck(Refs):-
    member(Ref,Refs),
    clause(program:Goal,Body,Ref),
    functor(Goal,Pred,Arity),
    if_option(checkcycles,clauselink(Pred/Arity,Pred/Arity)),
    copy_term(Goal,OGoal),
    gcheck_(Body,Pred/Arity,[t(Ref,OGoal,Goal,[])]).

gcheck_((Goal1 , Goal2),Target,Trail):-!,
    (gcheck_(Goal1,Target,Trail)
     ; 
     gcheck_(Goal2,Target,Trail)).

gcheck_((Goal1 ; Goal2),Target,Trail):-!,
    (gcheck_(Goal1,Target,Trail)
     ; 
     gcheck_(Goal2,Target,Trail)).

gcheck_((Cond -> Then ; Else),Target,Trail):-!,
    (gcheck_(Cond,Target,Trail)
     ;
     (gcheck_(Then,Target,Trail)
      ; 
      gcheck_(Else,Target,Trail))).

gcheck_(once(Goal),Target,Trail):-!,
    gcheck_(Goal,Target,Trail).

gcheck_(true,_,_):-!,fail.

% coinductive goal
gcheck_(Goal,Target,Trail):-
    if_option(checkcycles,(functor(Goal,Pred,Arity),clauselink(Pred/Arity,Target))),
    copy_term(Goal,OGoal),
    copy_term(Trail,OTrail),
    clause(program:Goal,Body,Ref),
    acyclic_term(Goal),
    clause(program:Head,_,Ref),
    loopcheck([t(Ref,OGoal,Goal,[])|Trail]),
    (subsumes_term(Head,OGoal)->
        gcheck_(Body,Target,[t(Ref,OGoal,Goal,[])|Trail])
        ;
        (cached_subterms(Ref,Head,Subterms),
         varcms(Goal,OGoal,Reducts),
         projs(Subterms,Reducts,Projs),
         goalsbyref(Ref,OTrail,Goals),
         gcontext(Projs,Goals,Gctx),
         copy_term(Gctx,Gctx_copy),
         (fixpoint(Ref,Gctx_copy,Trail) ->
          !,fail
          ;
          gcheck_(Body,Target,[t(Ref,OGoal,Goal,Gctx_copy)|Trail])
         )
        )
    ).
