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

:- dynamic option/1.

% helper predicates to handle options
handle_options(Options):-
    retractall(option(_)),
    forall(member(Option,Options),set_option(Option)).

unset_option(Option):-
    (-RawOption=Option;RawOption=Option),!,
    functor(RawOption,  Name, Arity),
    functor(Retract, Name, Arity),
    retractall(option(-Retract)),
    retractall(option(Retract)).

set_option(Option):-
    unset_option(Option),
    assert(option(Option)).

ifnot_option(Option,_Goal):-
    option(Option),!.

ifnot_option(Option,Goal):-
    option(-Option),!,
    call(Goal).

ifnot_option(Option,Goal):-
    not(default(Option)),
    call(Goal).

if_option(Option,Goal):-
    option(Option),!,
    call(Goal).

if_option(Option,Goal):-
    default(Option),
    not(option(-Option)),!,
    call(Goal).

if_option(_,_).
