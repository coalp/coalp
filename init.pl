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

% allow refering to current directory by coalp(_) for use_module
:- prolog_load_context(directory,Dir),asserta(user:file_search_path(coalp, Dir)). 

:- use_module(library(aggregate)).
:- use_module(library(lists)).

:- include(coalp(banner)).
:- include(coalp(builtin)).
:- include(coalp(load)).
:- include(coalp(guard)).
:- include(coalp(option)).
:- include(coalp(print)).
:- include(coalp(error)).
