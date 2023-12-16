// Signature file for parser generated by fsyacc
module Visp.Compiler.SyntaxParser
type token = 
  | SYMBOL of (string)
  | KEYWORD of (string)
  | RAWSTRING of (string)
  | DOT_METHOD of (string)
  | APPLY_METHOD of (string)
  | STRING of (string * SynStringKind * ParseHelpers.LexerContinuation)
  | CHAR of (string)
  | MACRO_NAME of (string)
  | PROP_PLUS of (string)
  | HASH_IDENT of (string)
  | DECIMAL of (System.Decimal)
  | IEEE64 of (double)
  | IEEE32 of (single)
  | UNATIVEINT of (uint64)
  | UINT64 of (uint64)
  | UINT32 of (uint32)
  | UINT16 of (uint16)
  | UINT8 of (byte)
  | NATIVEINT of (int64 * bool)
  | INT64 of (int64 * bool)
  | INT32 of (int32 * bool)
  | INT32_DOT_DOT of (int32 * bool)
  | INT16 of (int16 * bool)
  | INT8 of (sbyte * bool)
  | RETURN of (bool)
  | YIELD of (bool)
  | INLINE
  | REC
  | RINIT
  | THREAD_FIRST
  | THREAD_LAST
  | SYNTAX_MACRO
  | SEQ
  | UNION
  | ATOM_KW
  | DEREF_KW
  | WHILE
  | TYPE
  | RECORD
  | MEMBER
  | MEMBERS
  | MEMBERFN
  | MEMBER_GET
  | MEMBER_SET
  | OVERRIDE
  | MACRO
  | MATCH
  | WHEN
  | CONS
  | CONCAT
  | REQUIRE
  | FN
  | FNSTAR
  | LET
  | USE
  | LET_BANG
  | USE_BANG
  | DO_BANG
  | LETSTAR
  | MUT
  | SET
  | DOT
  | NEW
  | DOT_BRACKET
  | DOT_PLUS
  | IF_KW
  | BEGIN_KW
  | DO_KW
  | QUOTE_KW
  | UNQUOTE_KW
  | SPLICE_UNQUOTE_KW
  | QUASIQUOTE_KW
  | OPEN
  | MODULE
  | AT
  | DOTDOT
  | BANG_RANGE
  | FOR_IN
  | FOR_TO
  | BANG_LIST
  | BANG_MAP
  | BANG_SET
  | BANG_ARRAY
  | BANG_VEC
  | BANG_TUPLE
  | OP_PLUS
  | OP_MINUS
  | OP_MULT
  | OP_DIV
  | QUOTE_SYM
  | TRUE
  | FALSE
  | NIL
  | COLON
  | COLON_COLON
  | COMMA
  | UNIT
  | BAR
  | LBRACE
  | RBRACE
  | HASH_BRACE
  | HASH_PAREN
  | HASH_BRACKET
  | BRACE_BAR
  | BAR_BRACE
  | BRACKET_BAR
  | BAR_BRACKET
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | EOF
type tokenId = 
    | TOKEN_SYMBOL
    | TOKEN_KEYWORD
    | TOKEN_RAWSTRING
    | TOKEN_DOT_METHOD
    | TOKEN_APPLY_METHOD
    | TOKEN_STRING
    | TOKEN_CHAR
    | TOKEN_MACRO_NAME
    | TOKEN_PROP_PLUS
    | TOKEN_HASH_IDENT
    | TOKEN_DECIMAL
    | TOKEN_IEEE64
    | TOKEN_IEEE32
    | TOKEN_UNATIVEINT
    | TOKEN_UINT64
    | TOKEN_UINT32
    | TOKEN_UINT16
    | TOKEN_UINT8
    | TOKEN_NATIVEINT
    | TOKEN_INT64
    | TOKEN_INT32
    | TOKEN_INT32_DOT_DOT
    | TOKEN_INT16
    | TOKEN_INT8
    | TOKEN_RETURN
    | TOKEN_YIELD
    | TOKEN_INLINE
    | TOKEN_REC
    | TOKEN_RINIT
    | TOKEN_THREAD_FIRST
    | TOKEN_THREAD_LAST
    | TOKEN_SYNTAX_MACRO
    | TOKEN_SEQ
    | TOKEN_UNION
    | TOKEN_ATOM_KW
    | TOKEN_DEREF_KW
    | TOKEN_WHILE
    | TOKEN_TYPE
    | TOKEN_RECORD
    | TOKEN_MEMBER
    | TOKEN_MEMBERS
    | TOKEN_MEMBERFN
    | TOKEN_MEMBER_GET
    | TOKEN_MEMBER_SET
    | TOKEN_OVERRIDE
    | TOKEN_MACRO
    | TOKEN_MATCH
    | TOKEN_WHEN
    | TOKEN_CONS
    | TOKEN_CONCAT
    | TOKEN_REQUIRE
    | TOKEN_FN
    | TOKEN_FNSTAR
    | TOKEN_LET
    | TOKEN_USE
    | TOKEN_LET_BANG
    | TOKEN_USE_BANG
    | TOKEN_DO_BANG
    | TOKEN_LETSTAR
    | TOKEN_MUT
    | TOKEN_SET
    | TOKEN_DOT
    | TOKEN_NEW
    | TOKEN_DOT_BRACKET
    | TOKEN_DOT_PLUS
    | TOKEN_IF_KW
    | TOKEN_BEGIN_KW
    | TOKEN_DO_KW
    | TOKEN_QUOTE_KW
    | TOKEN_UNQUOTE_KW
    | TOKEN_SPLICE_UNQUOTE_KW
    | TOKEN_QUASIQUOTE_KW
    | TOKEN_OPEN
    | TOKEN_MODULE
    | TOKEN_AT
    | TOKEN_DOTDOT
    | TOKEN_BANG_RANGE
    | TOKEN_FOR_IN
    | TOKEN_FOR_TO
    | TOKEN_BANG_LIST
    | TOKEN_BANG_MAP
    | TOKEN_BANG_SET
    | TOKEN_BANG_ARRAY
    | TOKEN_BANG_VEC
    | TOKEN_BANG_TUPLE
    | TOKEN_OP_PLUS
    | TOKEN_OP_MINUS
    | TOKEN_OP_MULT
    | TOKEN_OP_DIV
    | TOKEN_QUOTE_SYM
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_NIL
    | TOKEN_COLON
    | TOKEN_COLON_COLON
    | TOKEN_COMMA
    | TOKEN_UNIT
    | TOKEN_BAR
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_HASH_BRACE
    | TOKEN_HASH_PAREN
    | TOKEN_HASH_BRACKET
    | TOKEN_BRACE_BAR
    | TOKEN_BAR_BRACE
    | TOKEN_BRACKET_BAR
    | TOKEN_BAR_BRACKET
    | TOKEN_LBRACKET
    | TOKEN_RBRACKET
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_EOF
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startraw_macro_body
    | NONTERM__startraw_expr
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_raw_expr
    | NONTERM_raw_macro_body
    | NONTERM_prog
    | NONTERM_file_fragments
    | NONTERM_rev_file_fragments
    | NONTERM_file_fragment
    | NONTERM_module_decls
    | NONTERM_rev_module_decls
    | NONTERM_module_decl
    | NONTERM_hash_ident_args
    | NONTERM_rev_hash_ident_args
    | NONTERM_hash_ident_arg
    | NONTERM_module_parens
    | NONTERM_module_help
    | NONTERM_expr_list
    | NONTERM_rev_expr_list
    | NONTERM_expr_list_or_empty
    | NONTERM_recover
    | NONTERM_expr
    | NONTERM_brace_bar
    | NONTERM_bracket_bar
    | NONTERM_lambda_short_expr_list
    | NONTERM_vector_start
    | NONTERM_inside_parens
    | NONTERM_parens_expr_start
    | NONTERM_dotted_exprs
    | NONTERM_rev_dotted_exprs
    | NONTERM_colon_colon_exprs
    | NONTERM_rev_colon_colon_exprs
    | NONTERM_parens_expr
    | NONTERM_macro_call_expr
    | NONTERM_macro_call
    | NONTERM_syntax_macro_expr
    | NONTERM_syntax_macro
    | NONTERM_macro_cases
    | NONTERM_rev_macro_cases
    | NONTERM_macro_case_start
    | NONTERM_macro_case_pat
    | NONTERM_macro_pat_list
    | NONTERM_rev_macro_pat_list
    | NONTERM_macro_pat
    | NONTERM_macro_body_list
    | NONTERM_rev_macro_body_list
    | NONTERM_collection_open_kind
    | NONTERM_macro_open_kind
    | NONTERM_collection_close_kind
    | NONTERM_macro_close_kind
    | NONTERM_macro_body
    | NONTERM_cons_expr
    | NONTERM_concat_expr
    | NONTERM_new_expr
    | NONTERM_threadable_list
    | NONTERM_rev_threadable_list
    | NONTERM_threadable
    | NONTERM_prop_plus
    | NONTERM_expr_comma_list
    | NONTERM_rev_expr_comma_list
    | NONTERM_dot_bracket_expr_raw
    | NONTERM_dot_expr
    | NONTERM_lparen_or_lbracket
    | NONTERM_rparen_or_rbracket
    | NONTERM_list_expr
    | NONTERM_for_in_expr
    | NONTERM_for_to_expr
    | NONTERM_range_expr
    | NONTERM_if_expr
    | NONTERM_quoted_exprs
    | NONTERM_rev_quoted_exprs
    | NONTERM_quoted_expr
    | NONTERM_quasiquoted_exprs
    | NONTERM_rev_quasiquoted_exprs
    | NONTERM_quasiquoted_expr
    | NONTERM_unquote
    | NONTERM_splice_unquote
    | NONTERM_operators
    | NONTERM_operators_not_in_parens
    | NONTERM_set
    | NONTERM_let
    | NONTERM_mut
    | NONTERM_letstar
    | NONTERM_type_expr
    | NONTERM_union_expr
    | NONTERM_union_labels_or_members
    | NONTERM_rev_union_labels_or_members
    | NONTERM_union_label_or_member_parens
    | NONTERM_union_label_or_member
    | NONTERM_union_fields
    | NONTERM_rev_union_fields
    | NONTERM_union_field
    | NONTERM_record_expr
    | NONTERM_record_labels_or_members
    | NONTERM_rev_record_labels_or_members
    | NONTERM_record_label_or_member_parens
    | NONTERM_record_label_or_member
    | NONTERM_typed_list
    | NONTERM_rev_typed_list
    | NONTERM_syntyped
    | NONTERM_record_members
    | NONTERM_record_init_expr
    | NONTERM_record_init_bar_list
    | NONTERM_rev_record_init_bar_list
    | NONTERM_record_init_init
    | NONTERM_record_init_list
    | NONTERM_rev_record_init_list
    | NONTERM_record_init_parens
    | NONTERM_record_init
    | NONTERM_attribute_list
    | NONTERM_rev_attribute_list
    | NONTERM_attr_list
    | NONTERM_attributes
    | NONTERM_rev_attributes
    | NONTERM_attribute
    | NONTERM_member_list_start
    | NONTERM_member_list
    | NONTERM_rev_member_list
    | NONTERM_member
    | NONTERM_member_name
    | NONTERM_member_get
    | NONTERM_member_set
    | NONTERM_member_member_in_parens
    | NONTERM_member_in_parens
    | NONTERM_tok_lparen
    | NONTERM_empty_bindings
    | NONTERM_binding_list_start
    | NONTERM_binding_list
    | NONTERM_rev_binding_list
    | NONTERM_binding
    | NONTERM_name
    | NONTERM_name_in_parens_or_brackets_start
    | NONTERM_name_in_parens_or_brackets
    | NONTERM_empty_name_list
    | NONTERM_name_list_start
    | NONTERM_name_list
    | NONTERM_rev_name_list
    | NONTERM_symbol
    | NONTERM_macro_name
    | NONTERM_dot_method
    | NONTERM_apply_method
    | NONTERM_keyword
    | NONTERM_syn_comma
    | NONTERM_raw_syntype_ident_text
    | NONTERM_raw_syntype_ident
    | NONTERM_syntype_ident
    | NONTERM_syntype_comma_list
    | NONTERM_rev_syntype_comma_list
    | NONTERM_constant
    | NONTERM_rawConstant
    | NONTERM_function_def
    | NONTERM_invalid_function_def
    | NONTERM_arg_list_start
    | NONTERM_arg_list
    | NONTERM_rev_arg_list
    | NONTERM_arg
    | NONTERM_arg_in_parens_or_brackets_start
    | NONTERM_arg_in_parens_or_brackets
    | NONTERM_function_call
    | NONTERM_function_call_args
    | NONTERM_empty_call_args
    | NONTERM_match_expr
    | NONTERM_match_list
    | NONTERM_rev_match_list
    | NONTERM_match_start
    | NONTERM_match
    | NONTERM_match_tuple_list
    | NONTERM_rev_match_tuple_list
    | NONTERM_match_pattern
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val raw_macro_body : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynMacroBody) 
val raw_expr : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (SynExpr) 
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (ParsedFile) 
