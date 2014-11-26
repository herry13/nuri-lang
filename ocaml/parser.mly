/**
 * Author: Herry (herry13@gmail.com)
 *
 * This is a parser which generates an abstract syntax tree from a concrete
 * syntax tree.
 */

%{

open Syntax

%}

%token <string> BOOL
%token <string> INT
%token <string> FLOAT
%token <string> STRING
%token <string> ID
%token NULL TOK_TBD TOK_UNKNOWN TOK_NOTHING
%token <string> INCLUDE_FILE
%token <string> NURI_INCLUDE_FILE
%token <string> IMPORT_FILE
%token EXTENDS COMMA DATA BEGIN END SEP
%token LBRACKET RBRACKET EOS EOF
%token ISA SCHEMA ENUM ASTERIX COLON TBOOL TINT TFLOAT TSTR TOBJ
%token GLOBAL SOMETIME ATLEAST ATMOST ALLDIFFERENT
%token TOK_NOT TOK_EQUAL_EQUAL TOK_PLUS
%token EQUAL NOT_EQUAL IF THEN ELSE IN NOT LPARENTHESIS RPARENTHESIS
%token TOK_GREATER TOK_GREATER_EQUAL TOK_LESS TOK_LESS_EQUAL
%token TOK_COLON_EQUAL
%token <string> SHELL
%token COST CONDITIONS EFFECTS ACTION

/* entry point to included file */
%token <Syntax.block -> Syntax.block> INCLUDE
%token <Syntax.context -> Syntax.context> NURI_INCLUDE

/**
 * entry point for:
 * - main-file is 'nuri'
 * - included file (at root) is 'incontext_included'
 * - included file (inside a block) is 'inblock_included'
 */
%start inblock_included nuri incontext_included
%type <Syntax.block -> Syntax.block> inblock_included
%type <Syntax.nuri> nuri
%type <Syntax.context -> Syntax.context> incontext_included

%%

nuri
	: context EOF { $1 EmptyContext }

incontext_included
	: context EOF { $1 }

context
	: SCHEMA schema context     { fun c -> SchemaContext ($2, $3 c) }
    | ENUM enum context         { fun c -> EnumContext ($2, $3 c) }
	| trajectory context        { fun c -> TrajectoryContext ($1, $2 c) }
	| NURI_INCLUDE EOS context  { fun c -> $1 ($3 c) }
	| assignment context        { fun c -> AssignmentContext ($1, $2 c) }
	|                           { fun c -> c }

inblock_included
	: block EOF { $1 }

block
	: assignment block   { fun b -> AssignmentBlock ($1, $2 b) }
	| trajectory block   { fun b -> TrajectoryBlock ($1, $2 b) }
	| INCLUDE EOS block  { fun b -> $1 ($3 b) }
	|                    { fun b -> b }

trajectory
    : GLOBAL global  { Global $2 }

assignment
	: ACTION reference action  { ($2, TUndefined, $3) }
	| reference type_def value { ($1, $2, $3) }

value
	: EQUAL simple_value EOS             { $2 }
	| TOK_COLON_EQUAL link_reference EOS { $2 }
	| ISA ID protos                      { Prototype (SID $2, $3) }
	| protos                             { Prototype (EmptySchema, $1) }

simple_value
    : TOK_TBD     { TBD }
    | TOK_UNKNOWN { Unknown }
    | TOK_NOTHING { Nothing }
    | exp         { Expression $1 }

exp
    : exp1                     { $1 }
    | TOK_NOT exp              { Exp_Not $2 }
    | IF exp THEN exp ELSE exp { IfThenElse ($2, $4, $6) }

exp1
    : exp2 binary_op         { $2 $1 }

binary_op
    : TOK_EQUAL_EQUAL exp2 binary_op { fun left -> $3 (Equal (left, $2)) }
    | TOK_PLUS exp2 binary_op        { fun left -> $3 (Add (left, $2)) }
    |                                { fun v -> v }

exp2
	: basic                         { Basic $1 }
    | func                          { $1 }
    | LPARENTHESIS exp RPARENTHESIS { $2 }

func
    : SHELL { Shell $1 }

protos
	: EXTENDS prototypes { $2 }
	| BEGIN block END    { BlockPrototype ($2 EmptyBlock, EmptyPrototype) }

prototypes
    : prototype COMMA prototypes { $1 $3 }
    | prototype                  { $1 EmptyPrototype }

prototype
    : BEGIN block END { fun p -> BlockPrototype ($2 EmptyBlock, p) }
    | reference       { fun p -> ReferencePrototype ($1, p) } 

basic
    : BOOL           { Boolean $1 }
    | INT            { Int $1 }
    | FLOAT          { Float $1 }
    | STRING         { String $1 }
    | data_reference { $1 }
    | NULL           { Null }
    | vector         { Vector $1 }

vector
    : LBRACKET items RBRACKET { $2 }

items
    : basic COMMA items { $1 :: $3 }
    | basic             { [$1] }

link_reference
    : reference { Link $1 }

data_reference
    : reference { Reference $1 }

reference
    : ID SEP reference { $1 :: $3 }
    | ID               { [$1] }

enum
    : ID BEGIN enum_elements END { ($1, $3) }

enum_elements
    : ID COMMA enum_elements { $1 :: $3 }
    | ID                     { [$1] }

schema
	: ID super BEGIN block END { ($1, $2, $4 EmptyBlock) }

super
	: EXTENDS ID { SID $2 }
	|            { EmptySchema }

type_def
	: COLON tau { $2 }
	|           { TUndefined }

tau
    : TBOOL                 { TBool }
    | TINT                  { TInt }
    | TFLOAT                { TFloat }
    | TSTR                  { TString }
    | LBRACKET RBRACKET tau { TList $3 }
    | ASTERIX tau_schema    { TRef $2 }
    | tau_schema            { TSchema $1 }

tau_schema
    : TOBJ  { TObject }
    | ID    { TUserSchema ($1, TObject) }

global
	: nuri_constraint { $1 }

conjunction
	: nuri_constraint conjunction { $1 :: $2 }
	|                            { [] }

disjunction
	: nuri_constraint disjunction { $1 :: $2 }
	|                            { [] }

nuri_constraint
	: BEGIN conjunction END                 { And $2 }
	| LPARENTHESIS disjunction RPARENTHESIS { Or $2 }
	| equal                                 { $1 }
	| equal_true                            { $1 }
	| not_equal                             { $1 }
	| negation                              { $1 }
	| implication                           { $1 }
	| membership                            { $1 }
	| greater_than                          { $1 }
	| less_than                             { $1 }
	| greater_equal                         { $1 }
	| less_equal                            { $1 }

equal
	: reference EQUAL basic EOS { Eq ($1, $3) }

equal_true
	: reference EOS { Eq ($1, Boolean "true") }

not_equal
	: reference NOT_EQUAL basic EOS { Ne ($1, $3) }

greater_than
	: reference TOK_GREATER basic EOS { Greater ($1, $3) }

greater_equal
	: reference TOK_GREATER_EQUAL basic EOS { GreaterEqual ($1, $3) }

less_than
	: reference TOK_LESS basic EOS { Less ($1, $3) }

less_equal
	: reference TOK_LESS_EQUAL basic EOS { LessEqual ($1, $3) }

implication
	: IF nuri_constraint THEN nuri_constraint { Imply ($2, $4) }

negation
	: NOT nuri_constraint { Not $2 }

membership
	: reference IN vector EOS { In ($1, $3) }

action
	: parameters BEGIN cost conditions EFFECTS BEGIN effects END END
		{ 
			Action ($1, $3, $4, $7)
		}

parameters
	: LPARENTHESIS params RPARENTHESIS { $2 }
	|                                  { [] }

params
	: param COMMA params { $1 :: $3 }
	| param              { [$1] }

param
	: ID COLON t_param { ($1, $3) }

t_param
    : tau_schema { TSchema $1 }
    | TBOOL      { TBool      }
    | TINT       { TInt       }
    | TFLOAT     { TFloat     }
    | TSTR       { TString    }

cost
	: COST EQUAL INT EOS { Cost $3 }
	|                    { EmptyCost }

conditions
	: CONDITIONS nuri_constraint { Condition $2 }
	|                           { EmptyCondition }

effects
	: effect effects { $1 :: $2 }
	| effect         { [$1] }

effect
	: reference EQUAL basic EOS { ($1, $3) }

%%
