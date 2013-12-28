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
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE LCHEVR RCHEVR SEP STAR HOLDER ANCHOR
%token PLUS MINUS VAR EQUAL
%token <int> CHAR MODIFIER
%token <string> IDENT GROUP_SUBSECT
%token <int> NUMBER
%token RULE_SECT DIALECT_SECT GROUP_SECT PERSISTENT_SECT

%start parse
%type <Data_set.rule_set> parse

%%

/* Lines */

newline:
| NEWLINE { }
| newline NEWLINE { $1 }
;

/* Binary features */

base_binary:
| base_binary PLUS IDENT { (Plus, $3) :: $1 }
| base_binary MINUS IDENT { (Minus, $3) :: $1 }
| { [] }
;

template_binary:
| template_binary PLUS IDENT { (Plus, $3) :: $1 }
| template_binary MINUS IDENT { (Minus, $3) :: $1 }
| template_binary VAR IDENT { (Variable, $3) :: $1 }
| { [] }
;

modifiers:
| modifiers MODIFIER { (from_modifier $2) :: $1 }
| { [] }
;

/* Matching location related */

matching_t:
| CHAR modifiers { Atom (extend (modify (from_char $1) $2)) }
| LBRACK base_binary RBRACK { Atom (from_phoneme_list $2) }
| LCHEVR base_binary RCHEVR { Atom (from_suprasegmental_list $2) }
;

/*matching_group:
| matching_group matching_t { $2 :: $1 }
| matching_group LPAREN matching_group RPAREN STAR { (Str (Seq (List.rev $3))) :: $1 }
| matching_group LPAREN matching_group RPAREN { (Opt (Seq (List.rev $3))) :: $1 }
| { [] }
;*/

matching: /* FIXME: matching vs matching_group */
| matching matching_t { $2 :: $1 }
| matching LPAREN matching RPAREN { (Opt (Seq (List.rev $3))) :: $1 }
| matching LPAREN matching RPAREN STAR { (Str (Seq (List.rev $3))) :: $1 }
| matching LBRACE NUMBER matching RBRACE { (Grp (Seq (List.rev $4), Number $3)) :: $1 }
| { [] }
;

/* Environment location related */

environment_t:
| CHAR modifiers { Atom (extend (modify (from_char $1) $2)) }
| LBRACK base_binary RBRACK { Atom (from_phoneme_list $2) }
| LCHEVR base_binary RCHEVR { Atom (from_suprasegmental_list $2) }
| LBRACE base_binary RBRACE { Grp (Atom (from_phoneme_list $2), Dynamic)}
;

environment:
| environment environment_t { $2 :: $1 }
| environment LPAREN environment RPAREN STAR { (Str (Seq (List.rev $3))) :: $1 }
| environment LPAREN environment RPAREN { (Opt (Seq (List.rev $3))) :: $1 }
| { [] }
;

environment_before:
| ANCHOR environment { (true, Seq (List.rev $2)) }
| environment { (false, Seq (List.rev $1)) }
;

environment_after:
| environment ANCHOR { (true, Seq (List.rev $1)) }
| environment { (false, Seq (List.rev $1)) }
;

/* Template location related */

template_t:
| CHAR modifiers { Char (extend (modify (from_char $1) $2)) }
| LBRACK template_binary RBRACK { Char (from_phoneme_list $2) }
| LCHEVR template_binary RCHEVR { Char (from_suprasegmental_list $2) }
;

template:
| template template_t { $2 :: $1 }
| template LBRACE NUMBER template RBRACE { (Group ($3, (match $4 with [] -> None | _ -> Some (List.rev $4)))) :: $1 }
| { [] }
;


/* Dialects */

dialects:
| dialects SEP IDENT { $3 :: $1 }
| { [] }
;

/* Core rule */

rule:
| matching SEP template SEP environment_before HOLDER environment_after
  {
    let (is_start, before) = $5
    and (is_end, after) = $7
    and matching = Seq (List.rev $1) in
    create_rule
      (is_start, is_end)
      (simplify before, simplify matching, simplify after)
      (List.rev $3)
  }
;

abstr_rule:
| IDENT dialects { Rule_group ($1, $2) }
| rule dialects { Rule ($1, $2) }
;

/* Rule section */

rule_sublines:
| rule_sublines abstr_rule newline { $2 :: $1 }
| { [] }
;

rule_lines:
| rule_lines NUMBER newline rule_sublines { ($2, List.rev $4) :: $1 }
| { [] }
;

rule_section:
| RULE_SECT newline rule_lines { List.rev $3 }
;

/* Persistent section */

persistent_section:
| PERSISTENT_SECT newline rule_sublines { List.rev $3 }
;

/* Group section */

group_sublines:
| group_sublines rule newline { $2 :: $1 }
| { [] }
;

group_lines:
| group_lines GROUP_SUBSECT newline group_sublines { ($2, List.rev $4) :: $1 }
| { [] }
;

group_section:
| GROUP_SECT newline group_lines { $3 }
;

/* Dialect section */

dialect_lines:
| dialect_lines IDENT EQUAL IDENT newline { ($2, $4) :: $1 }
| { [] }
;

dialect_section:
| DIALECT_SECT newline dialect_lines { $3 }
;

/* Main part */

sections:
| sections rule_section { (`Rule $2) :: $1 }
| sections persistent_section { (`Persistent $2) :: $1 }
| sections dialect_section { (`Dialect $2) :: $1 }
| sections group_section { (`Group $2) :: $1 }
| newline { [] }
| { [] }
;

parse:
| sections EOF
  {
    let rule = match List.find_all (function `Rule _ -> true | _ -> false) $1 with
    | [] -> raise Missing_section_rules
    | [`Rule t] -> t
    | _ -> raise (Too_many_sections "Rules")
    and persistent = match List.find_all (function `Persistent _ -> true | _ -> false) $1 with
    | [] -> []
    | [`Persistent t] -> t
    | _ -> raise (Too_many_sections "Persistent")
    and group = match List.find_all (function `Group _ -> true | _ -> false) $1 with
    | [] -> []
    | [`Group t] -> t
    | _ -> raise (Too_many_sections "Groups")
    and dialect = match List.find_all (function `Dialect _ -> true | _ -> false) $1 with
    | [] -> []
    | [`Dialect t] -> t
    | _ -> raise (Too_many_sections "Dialects")
    in
    create_ruleset rule persistent group dialect
  }
;

%%
