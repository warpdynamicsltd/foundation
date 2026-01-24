%{
  open Types
  (*open Errors*)

  (*let cx_error msg pos =
    (CxError (msg ^ " " ^ Parser_utils.location_to_string pos))*)

%}

%token <string> UWORD
%token <string> LWORD
%token <int> INT
%token COMMA DOT COLON LPAREN RPAREN SEMICOLON
%token LCURL RCURL
%token AND OR NOT IMPLIES IFF
%token FORALL EXISTS
%token TRUE FALSE
%token EQUAL
%token LBRACK RBRACK
%token TERM_PREFIX FORMULA_PREFIX
%token EOF

%start input
%start wff
%start wft
%start ref
%start statement
%start statements
%type <Types.statement>      input
%type <Types.first_order_formula> wff
%type <Types.term>                wft
%type <Types.reference>           ref
%type <Types.statement>           statement
%type <Types.statement list>      statements
%%

input:
  statement EOF {$1}

wff:
  fof_formula EOF {$1}

wft:
  term EOF {$1}

ref:
  reference EOF {$1}

statements:
| statement { [$1] }
| statement statements { $1 :: $2 }

statement:
| ref=reference formula=fof_formula rule=rule_arg SEMICOLON
        { Statement {ref; formula; statements=[||]; inference=Inference{ rule; refs=[]}; pos=($startpos) } }
| ref=reference formula=fof_formula rule=rule_arg refs=refs_arg SEMICOLON
        { Statement {ref; formula; statements=[||]; inference=Inference{ rule; refs}; pos=($startpos) } }
| ref=reference formula=fof_formula LCURL statements=statements RCURL
        { Statement {ref; formula; statements=Array.of_list statements; inference=Context; pos=($startpos) } }

(*| error { raise (cx_error "expected statement" $startpos) }*)

reference:
  integers { Ref $1 }

integers:
| DOT {[]}
| INT { [$1] }
| INT DOT integers { $1 :: $3 }

rule:
  | name=UWORD {Rule {name; params=[]}}
  | name=UWORD LPAREN params=params RPAREN {Rule {name; params}}

rule_arg:
  | LCURL rule RCURL { $2 }

param:
  | TERM_PREFIX term { Term $2 }
  | FORMULA_PREFIX fof_formula { Formula $2 }

params:
  | param {[$1]}
  | param SEMICOLON params {$1 :: $3}

refs:
  | reference {[$1]}
  | reference SEMICOLON refs {$1 :: $3}

refs_arg:
  LCURL RCURL { [] }
| LCURL refs RCURL { $2 }

fof_formula:
| primary AND primary               { And($1, $3) }
| primary OR primary                { Or($1, $3) }
| primary IMPLIES primary           { Implies($1, $3) }
| primary IFF primary               { Iff($1, $3) }
| primary                           { $1 }
(*| error { raise (cx_error "expected formula" $startpos) }*)

primary:
| atom { $1 }
| NOT primary                       { Not $2 }
| LPAREN fof_formula RPAREN { $2 }
| FORALL LBRACK vars RBRACK COLON primary
    { List.fold_right (fun v acc -> Forall(v, acc)) $3 $6 }
| EXISTS LBRACK vars RBRACK COLON primary
    { List.fold_right (fun v acc -> Exists(v, acc)) $3 $6 }

atomic_name:
  LWORD                        { $1 }

predicate:
  atomic_name LPAREN terms RPAREN { Pred($1, $3) }
| atomic_name                      { Pred($1, []) }

atom:
  TRUE                                     { True }
| FALSE                                    { False }
| predicate                                { $1 }
| term EQUAL term                          { Pred("=", [$1; $3]) }

vars:
  var COMMA vars             { $1 :: $3 }
| var                        { [$1] }

var:
  UWORD                        { $1 }

const:
  atomic_name                 { $1 }

terms:
  term COMMA terms             { $1 :: $3 }
| term                         { [$1] }
(*| error { raise (cx_error "expected terms" $startpos) }*)

term:
  atomic_name LPAREN terms RPAREN     { Func($1, $3) }
| var                                 { Var $1 }
| const                               { Const $1 }
(*| error { raise (cx_error "expected term" $startpos) }*)
