/* MU Core Syntax in YACC style. */
%token BV
%token RV

%%

F : 'E' BV '.' '(' F ')'
  | 'D' BV '.' '(' F ')'
  | '(' F '+' F ')'
  | '~' '(' F ')'
  | '[' T ']' '(' FL ')'
  | '0' | '1' | BV
  ;

FL : F | FL ',' F ;

T : 'L' BVL '.' '(' F ')'
  | 'mu' RV '.' '[' T ']'
  | '[' T '+' T ']'
  | '~' '[' T ']'
  | '0' | '1' | RV
  ;

BVL : BV | BV ',' BVL ;
