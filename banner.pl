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


print_banner :- 
    coalp_version(Version),
    coalp_copyright_year(Year),
    format('CoALP version ~s , Copyright (c) ~s The CoALP Authors.\n',[Version,Year]),
    print_welcome.

print_welcome :- 
    writeln("type examples. to see example queries").

coalp_version("0.1.0 beta"). 
coalp_copyright_year("2016").

% predicate to show example queries when invoked    
examples :-
    nl,
    format("?- load(\"examples/from.logic\").\n",[]),
    format("   -> loads the logic program examples/from.logic and checks global productivity.\n\n",[]),
    format("?- query(from(0,X)).\n",[]),
    format("   -> queries the loaded program with the goal from(0,X).\n\n",[]).
