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
%token <string> ISTRING
%token <string> ID
%token NULL TOK_TBD TOK_UNKNOWN TOK_NONE
%token <string> INCLUDE_FILE
%token <string> IMPORT_FILE
%token EXTENDS COMMA DATA BEGIN END SEP
%token LBRACKET RBRACKET EOS EOF
%token ISA SCHEMA ENUM ASTERIX COLON TBOOL TINT TFLOAT TSTR TOBJ
%token GLOBAL SOMETIME ATLEAST ATMOST ALLDIFFERENT
%token TOK_EQUAL_EQUAL TOK_AND TOK_OR TOK_IMPLY TOK_EXCLAMATION TOK_DOLLAR
%token TOK_PLUS TOK_SUBTRACT TOK_DIVIDER TOK_MODULO
%token EQUAL NOT_EQUAL IF THEN ELSE IN NOT LPARENTHESIS RPARENTHESIS
%token TOK_GREATER TOK_GREATER_EQUAL TOK_LESS TOK_LESS_EQUAL
%token TOK_COLON_EQUAL
%token <string> SHELL
%token <string> REGEXP
%token COST CONDITIONS EFFECTS ACTION
%token <string> HASH_ECHO

/* entry point to included file */
%token <Syntax.block -> Syntax.block> INCLUDE
%token <Syntax.context -> Syntax.context> IMPORT

/**
 * entry point for:
 * - main-file is 'nuri'
 * - import file (at root) is 'import_file'
 * - included file (inside a block) is 'include_file'
 */
%start nuri import_file include_file
%type <Syntax.nuri> nuri
%type <Syntax.context -> Syntax.context> import_file
%type <Syntax.block -> Syntax.block> include_file

%%

nuri
	: context EOF { $1 EmptyContext }

import_file
    : context EOF { $1 }

include_file
	: block EOF { $1 }

context
	: SCHEMA schema context { fun c -> SchemaContext ($2, $3 c) }
    | ENUM enum context     { fun c -> EnumContext ($2, $3 c) }
	| trajectory context    { fun c -> TrajectoryContext ($1, $2 c) }
	| IMPORT EOS context    { fun c -> $1 ($3 c) }
	| assignment context    { fun c -> AssignmentContext ($1, $2 c) }
	|                       { fun c -> c }

block
	: assignment block   { fun b -> AssignmentBlock ($1, $2 b) }
	| trajectory block   { fun b -> TrajectoryBlock ($1, $2 b) }
	| INCLUDE EOS block  { fun b -> $1 ($3 b) }
	|                    { fun b -> b }

trajectory
    : GLOBAL global  { Global $2 }

assignment
	: ACTION reference action  { ($2, T_Undefined, $3) }
	| reference type_def value { ($1, $2, $3) }
    | HASH_ECHO value          { ([$1], T_Undefined, $2) }

value
	: simple_value EOS                   { $1 }
	| TOK_COLON_EQUAL link_reference EOS { $2 }
	| ISA ID protos                      { Prototype (SID $2, $3) }
	| protos                             { Prototype (EmptySchema, $1) }

simple_value
    : TOK_TBD     { TBD }
    | TOK_UNKNOWN { Unknown }
    | TOK_NONE    { None }
    | exp         { Expression $1 }

exp
    : exp1                     { $1 }
    | TOK_EXCLAMATION exp      { Exp_Not $2 }
    | TOK_DOLLAR exp           { Exp_Eager $2 }
    | IF exp THEN exp ELSE exp { Exp_IfThenElse ($2, $4, $6) }

exp1
    : exp2 binary_op { $2 $1 }

binary_op
    : TOK_EQUAL_EQUAL exp2 binary_op { fun left -> $3 (Exp_Equal (left, $2)) }
    | NOT_EQUAL exp2 binary_op       { fun left -> $3 (Exp_NotEqual (left, $2)) }
    | TOK_PLUS exp2 binary_op        { fun left -> $3 (Exp_Add (left, $2)) }
    | TOK_SUBTRACT exp2 binary_op    { fun left -> $3 (Exp_Subtract (left, $2)) }
    | ASTERIX exp2 binary_op         { fun left -> $3 (Exp_Multiply (left, $2)) }
    | TOK_DIVIDER exp2 binary_op     { fun left -> $3 (Exp_Divide (left, $2)) }
    | TOK_MODULO exp2 binary_op      { fun left -> $3 (Exp_Modulo (left, $2)) }
    | TOK_AND exp2 binary_op         { fun left -> $3 (Exp_And (left, $2)) }
    | TOK_OR exp2 binary_op          { fun left -> $3 (Exp_Or (left, $2)) }
    | TOK_IMPLY exp2 binary_op       { fun left -> $3 (Exp_Imply (left, $2)) }
    | REGEXP binary_op               { fun left -> $2 (Exp_MatchRegexp (left, $1)) }
    |                                { fun v -> v }

exp2
	: basic                         { Basic $1 }
    | func                          { $1 }
    | LPARENTHESIS exp RPARENTHESIS { $2 }

func
    : SHELL   { Shell $1 }
    | ISTRING { Exp_IString $1 }

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
	|           { T_Undefined }

tau
    : TBOOL                 { T_Bool }
    | TINT                  { T_Int }
    | TFLOAT                { T_Float }
    | TSTR                  { T_String }
    | LBRACKET RBRACKET tau { T_List $3 }
    | ASTERIX tau_object    { T_Reference $2 }
    | tau_object            { T_Object $1 }

tau_object
    : TOBJ  { T_PlainObject }
    | ID    { T_Schema ($1, T_PlainObject) }

global
	: nuri_constraint { $1 }

conjunction
	: nuri_constraint conjunction { $1 :: $2 }
	|                             { [] }

disjunction
	: nuri_constraint disjunction { $1 :: $2 }
	|                             { [] }

nuri_constraint
	: BEGIN conjunction END                 { C_And $2 }
	| LPARENTHESIS disjunction RPARENTHESIS { C_Or $2 }
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
	: reference EQUAL basic EOS { C_Equal ($1, $3) }

equal_true
	: reference EOS { C_Equal ($1, Boolean "true") }

not_equal
	: reference NOT_EQUAL basic EOS { C_NotEqual ($1, $3) }

greater_than
	: reference TOK_GREATER basic EOS { C_Greater ($1, $3) }

greater_equal
	: reference TOK_GREATER_EQUAL basic EOS { C_GreaterEqual ($1, $3) }

less_than
	: reference TOK_LESS basic EOS { C_Less ($1, $3) }

less_equal
	: reference TOK_LESS_EQUAL basic EOS { C_LessEqual ($1, $3) }

implication
	: IF nuri_constraint THEN nuri_constraint { C_Imply ($2, $4) }

negation
	: NOT nuri_constraint { C_Not $2 }

membership
	: reference IN vector EOS { C_In ($1, $3) }

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
    : tau_object { T_Object $1 }
    | TBOOL      { T_Bool      }
    | TINT       { T_Int       }
    | TFLOAT     { T_Float     }
    | TSTR       { T_String    }

cost
	: COST INT EOS { Cost $2 }
	|              { EmptyCost }

conditions
	: CONDITIONS nuri_constraint { Condition $2 }
	|                            { EmptyCondition }

effects
	: effect effects { $1 :: $2 }
	| effect         { [$1] }

effect
	: reference basic EOS { ($1, $2) }

%%
