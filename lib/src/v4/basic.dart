// ignore_for_file: non_constant_identifier_names

/*
 * This file is part of the ANTLR v4, Token Parser
 * Referenced from LexBasic.g4 (https://github.com/antlr/grammars-v4/blob/master/antlr/antlr4/LexBasic.g4)
 */

import 'package:token_parser/token_parser.dart';

/* -= Whitespace & Comments =- */

final Ws = Hws | Vws;

final Hws = ' ' | '\t';

final Vws = '\r' | '\n' | '\f';

final BlockComment = '/*' & anyUntil('*/').optional & ('*/' | end);

final DocComment = '/**' & anyUntil('*/').optional & ('*/' | end);

final LineComment = '//' & anyUntil('\r' | '\n').optional & (('\r' | '\n') | end);

/* -= Escapes =- */

final EscSeq = Esc & ( 'b' | 't' | 'n' | 'f' | 'r' | '"' | "'" | '\\' | UnicodeEsc | any() | end);

final EscAny = Esc & any();

final UnicodeEsc = 'u' & HexDigit.repeat(0, 4);

/* -= Numerals =- */

final DecimalNumeral = '0' | ('[1-9]'.regex & DecDigit.multiple.optional);

final HexDigit = '[0-9a-fA-F]'.regex;

final DecDigit = '[0-9]'.regex;

/* -= Literals =- */

final BoolLiteral = 'true' | 'false';

final CharLiteral = SQuote & (EscSeq | -('\r' | '\n' | '\\' | SQuote)) & SQuote;

final SQuoteLiteral = SQuote & (EscSeq | -('\r' | '\n' | '\\' | SQuote)).multiple.optional & SQuote;

final DQuoteLiteral = DQuote & (EscSeq | -('\r' | '\n' | '\\' | DQuote)).multiple.optional & DQuote;

final USQuoteLiteral = SQuote & (EscSeq | -('\r' | '\n' | '\\')).multiple.optional;

/* -= Character Ranges =- */

final NameChar =
  NameStartChar
  | '[0-9]'.regex
  | Underscore
  | '\u00B7'
  | '[\u0300-\u036F]'.regex
  | '[\u203F-\u2040]'.regex;

final NameStartChar = 
  '[A-Z]'.regex
  | '[a-z]'.regex
  | '[\u00C0-\u00D6]'.regex
  | '[\u00D8-\u00F6]'.regex
  | '[\u00F8-\u02FF]'.regex
  | '[\u0370-\u037D]'.regex
  | '[\u037F-\u1FFF]'.regex
  | '[\u200C-\u200D]'.regex
  | '[\u2070-\u218F]'.regex
  | '[\u2C00-\u2FEF]'.regex
  | '[\u3001-\uD7FF]'.regex
  | '[\uF900-\uFDCF]'.regex
  | '[\uFDF0-\uFFFD]'.regex;

/* -= Types =- */

final Int = 'int'.lexeme();

/* -= Symbols =- */

final Esc = '\\'.lexeme();

final Colon = ':'.lexeme();

final DColon = '::'.lexeme();

final SQuote = "'".lexeme();

final DQuote = '"'.lexeme();

final LParen = '('.lexeme();

final RParen = ')'.lexeme();

final LBrace = '{'.lexeme();

final RBrace = '}'.lexeme();

final LBrack = '['.lexeme();

final RBrack = ']'.lexeme();

final RArrow = '->'.lexeme();

final Lt = '<'.lexeme();

final Gt = '>'.lexeme();

final Equal = '='.lexeme();

final Question = '?'.lexeme();

final Star = '*'.lexeme();

final Plus = '+'.lexeme();

final PlusAssign = '+='.lexeme();

final Underscore = '_'.lexeme();

final Pipe = '|'.lexeme();

final Dollar = '\$'.lexeme();

final Comma = ','.lexeme();

final Semi = ';'.lexeme();

final Dot = '.'.lexeme();

final Range = '..'.lexeme();

final At = '@'.lexeme();

final Pound = '#'.lexeme();

final Tilde = '~'.lexeme();