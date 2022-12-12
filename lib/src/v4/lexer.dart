// ignore_for_file: non_constant_identifier_names

/*
 * This file is part of the ANTLR v4, Token Parser
 * Referenced from ANTLRv4Lexer.g4 (https://github.com/antlr/grammars-v4/blob/master/antlr/antlr4/ANTLRv4Lexer.g4)
 */

import 'package:token_parser/token_parser.dart';
import 'package:antlr_parser/src/v4/basic.dart';

/* [UNSUPPORTED]
tokens { TOKEN_REF, RULE_REF, LEXER_CHAR_SET }
channels { OFF_CHANNEL, COMMENT }
*/

/* -= Comments =- */

final DOC_COMMENT = DocComment; // [UNSUPPORTED] -> channel (COMMENT)

final BLOCK_COMMENT = BlockComment; // [UNSUPPORTED] -> channel (COMMENT)

final LINE_COMMENT = LineComment; // [UNSUPPORTED] -> channel (COMMENT)

// Extra - Group all comments
final COMMENT = DocComment | BlockComment | LineComment;

/* -= Integer =- */

final INT = DecimalNumeral;

/* -= Literal Strings =- */

final STRING_LITERAL = SQuoteLiteral;

final UNTERMINATED_STRING_LITERAL = USQuoteLiteral;

/* -= Arguments =- */

final BEGIN_ARGUMENT = LBrack; // [UNSUPPORTED] { this.handleBeginArgument(); }

final BEGIN_ACTION = LBrace; // [UNSUPPORTED] -> pushMode (TargetLanguageAction)

/* -= Keywords =- */

final OPTIONS = 'options' & WSNLCHARS.multiple.optional & '{';

final TOKENS = 'tokens' & WSNLCHARS.multiple.optional & '{';

final CHANNELS = 'channels' & WSNLCHARS.multiple.optional & '{';

final WSNLCHARS = ' ' | '\t' | '\f' | '\n' | '\r';

final IMPORT = 'import'.lexeme();

final FRAGMENT = 'fragment'.lexeme();

final LEXER = 'lexer'.lexeme();

final PARSER = 'parser'.lexeme();

final GRAMMAR = 'grammar'.lexeme();

final PROTECTED = 'protected'.lexeme();

final PUBLIC = 'public'.lexeme();

final PRIVATE = 'private'.lexeme();

final RETURNS = 'returns'.lexeme();

final LOCALS = 'locals'.lexeme();

final THROWS = 'throws'.lexeme();

final CATCH = 'catch'.lexeme();

final FINALLY = 'finally'.lexeme();

final MODE = 'mode'.lexeme();

final COLON = Colon;

final COLONCOLON = DColon;

final COMMA = Comma;

final SEMI = Semi;

final LPAREN = LParen;

final RPAREN = RParen;

final LBRACE = LBrace;

final RBRACE = RBrace;

final RARROW = RArrow;

final LT = Lt;

final GT = Gt;

final ASSIGN = Equal;

final QUESTION = Question;

final STAR = Star;

final PLUS_ASSIGN = PlusAssign;

final PLUS = Plus;

final OR = Pipe;

final DOLLAR = Dollar;

final RANGE = Range;

final DOT = Dot;

final AT = At;

final POUND = Pound;

final NOT = Tilde; 

/* -= Identifiers =- */

final ID = Id;

/* -= Whitespace =- */

final WS = Ws.multiple; // [UNSUPPORTED] -> channel (OFF_CHANNEL)

/* -= Illegal Characters =- */

final ERRCHAR = any(); // [UNSUPPORTED] -> channel (HIDDEN)

/* -= Arguments =- */

final NESTED_ARGUMENT = LBrack; // [UNSUPPORTED] -> type (ARGUMENT_COMMENT) , pushMode (NestedArgument)

final ARGUMENT_ESCAPE = EscAny; // [UNSUPPORTED] -> type (ARGUMENT_COMMENT)

final ARGUMENT_STRING_LITERAL = DQuoteLiteral; // [UNSUPPORTED] -> type (ARGUMENT_COMMENT)

final ARGUMENT_CHAR_LIETERAL = SQuoteLiteral; // [UNSUPPORTED] -> type (ARGUMENT_COMMENT)

final END_ARGUMENT = RBrack; // [UNSUPPORTED] -> { this.handleEndArgument(); }

final UNTERMINATED_ARGUMENT = end; // [UNSUPPORTED] -> popMode

final ARGUMENT_CONTENT = any();

/* -= Target Lenguage Actions =- */

final NESTED_ACTION = LBrace; // [UNSUPPORTED] -> type (ACTION_COMMENT) , pushMode (TargetLanguageAction)

final ACTION_ESCAPE = EscAny; // [UNSUPPORTED] -> type (ACTION_COMMENT)

final ACTION_STRING_LITERAL = DQuoteLiteral; // [UNSUPPORTED] -> type (ACTION_COMMENT)

final ACTION_CHAR_LIETERAL = SQuoteLiteral; // [UNSUPPORTED] -> type (ACTION_COMMENT)

final ACTION_DOC_COMMENT = DocComment; // [UNSUPPORTED] -> type (ACTION_COMMENT)

final ACTION_BLOCK_COMMENT = BlockComment; // [UNSUPPORTED] -> type (ACTION_COMMENT)

final ACTION_LINE_COMMENT = LineComment; // [UNSUPPORTED] -> type (ACTION_COMMENT)

final END_ACTION = RBrace; // [UNSUPPORTED] { this.handleEndAction(); }

final UNTERMINATED_ACTION = end; // [UNSUPPORTED] -> popMode

final ACTION_CONTENT = anyUntil(END_ACTION);

/* -=- */

final LEXER_CHAR_SET_BODY = (-(']' | '\\') | EscAny).multiple; // [UNSUPPORTED] -> more

final LEXER_CHAR_SET = LBrack & LEXER_CHAR_SET_BODY & RBrack; // [MODIFIED] [UNSUPPORTED] -> popMode

final UNTERMINATED_CHAR_SET = end; // [UNSUPPORTED] -> popMode

/* -=- */

final Id = NameStartChar & NameChar.multiple;

/* -= Extra - From ANTLRv2Lexer.g4 =- */

final TOKEN_REF = '[A-Z]'.regex & ('[a-zA-Z]'.regex | '_' | '[0-9]'.regex).multiple.optional;

final RULE_REF = '[a-z]'.regex & ('[a-zA-Z]'.regex | '_' | '[0-9]'.regex).multiple.optional;
