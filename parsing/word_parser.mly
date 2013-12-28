/* Copyright 2007 Pierre-Marie Pédrot */
/*
    This file is part of zamel.

    zamel is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    zamel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

%{
open Binary
open Regexp
open Data_set
%}

%token NEWLINE EOF
%token <int> CHAR MODIFIER

%start parse
%type <Data_set.lexicon> parse

%%

/* Lines */

newline:
| NEWLINE { }
| newline NEWLINE { $1 }
;

/* Word */

word:
| word CHAR modifiers { (modify (from_char $2) $3) :: $1 }
| CHAR modifiers { [modify (from_char $1) $2] }
;

modifiers:
| modifiers MODIFIER { (from_modifier $2) :: $1 }
| { [] }
;

/* Main */

lexicon:
| lexicon word newline { (List.rev $2) :: $1 }
| newline { [] }
| { [] }
;

parse:
| lexicon EOF { List.rev $1 }
;

%%
