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

catch(Goal):-
    catch(Goal,error(Error),handle_error(Error)).

handle_error(loop(Trail)):-!,
    reverse(Trail,Path),
    Path=[t(_,Goal,_,_)|_],
    format("Goal ~w results in unguarded loop in path [",[Goal]),
    print_path(Path),
    format("].",[]),
    nl,
    fail.

handle_error(Error):-
    print(Error),
    fail.