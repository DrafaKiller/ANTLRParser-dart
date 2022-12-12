// ignore_for_file: non_constant_identifier_names

/*
 * This file is part of the ANTLR v4, Token Parser
 * Referenced from ANTLRv4Parser.g4 (https://github.com/antlr/grammars-v4/blob/master/antlr/antlr4/ANTLRv4Parser.g4)
 */

import 'package:token_parser/token_parser.dart';
import 'package:antlr_parser/src/v4/lexer.dart';

/* [UNSUPPORTED]
options { tokenVocab=ANTLRv4Lexer; }
*/

final grammarSpec = grammarDecl * prequelConstruct.multiple.optional * rules * modeSpec.multiple.optional;

final grammarDecl = grammarType + identifier * SEMI;

final grammarType = (LEXER + GRAMMAR) | (PARSER + GRAMMAR) | GRAMMAR;

final prequelConstruct =
  optionsSpec
  | delegateGrammars
  | tokensSpec
  | channelsSpec
  | action_;

/* -= Options =- */
  
final optionsSpec = OPTIONS * (option * SEMI).multiple.optional * RBRACE;

final option = identifier * ASSIGN * optionValue;

final optionValue =
  (identifier * (DOT * identifier).multiple.optional)
  | STRING_LITERAL
  | actionBlock
  | INT;
  
/* -= Delegates =- */

final delegateGrammars = IMPORT + delegateGrammar * (COMMA * delegateGrammar).multiple.optional * SEMI;

final delegateGrammar =
  (identifier * ASSIGN * identifier)
  | identifier;

/* -= Tokens & Channels =- */

final tokensSpec = TOKENS * idList.optional * RBRACE;

final channelsSpec = CHANNELS * idList.optional * RBRACE;

final idList = identifier * (COMMA * identifier).multiple.optional * COMMA.optional;

final action_ = AT & (actionScopeName * COLONCOLON).optional * identifier * actionBlock;

final actionScopeName =
  identifier
  | LEXER
  | PARSER;

final actionBlock = BEGIN_ACTION * ACTION_CONTENT.multiple.optional * END_ACTION;

final argActionBlock = BEGIN_ARGUMENT * ARGUMENT_CONTENT.multiple.optional * END_ARGUMENT;

final modeSpec = MODE + identifier * SEMI * lexerRuleSpec.multiple.optional;

final rules = (~ruleSpec).multiple.optional;

final ruleSpec = parserRuleSpec | lexerRuleSpec;

final parserRuleSpec =
  ruleModifiers.optional * RULE_REF * argActionBlock.optional * ruleReturns.optional * throwsSpec.optional
  * localsSpec.optional * rulePrequel.multiple.optional * COLON * ruleBlock * SEMI * exceptionGroup;

final exceptionGroup = exceptionHandler.multiple.optional * finallyClause.optional;

final exceptionHandler = CATCH * argActionBlock * actionBlock;

final finallyClause = FINALLY * actionBlock;

final rulePrequel = optionsSpec | ruleAction;

final ruleReturns = RETURNS * argActionBlock;

/* -= Exception spec =- */

final throwsSpec = THROWS + identifier * (COMMA * identifier).multiple.optional;

final localsSpec = LOCALS * argActionBlock;

final ruleAction = AT * identifier * actionBlock;

final ruleModifiers = ruleModifier.multiple;

final ruleModifier =
  PUBLIC
  | PRIVATE
  | PROTECTED
  | FRAGMENT;

final ruleBlock = ruleAltList;

final ruleAltList = labeledAlt * (OR * labeledAlt).multiple.optional;

final labeledAlt = alternative * (POUND & identifier).optional;

/* -= Lexer rules =- */

final lexerRuleSpec = FRAGMENT.optional * TOKEN_REF * optionsSpec.optional * COLON * lexerRuleBlock * SEMI;

final lexerRuleBlock = lexerAltList;

final lexerAltList = lexerAlt * (OR * lexerAlt).multiple.optional;

final lexerAlt = 
  (lexerElements * lexerCommands.optional)
  | empty();

final lexerElements = lexerElement.multiple | empty();

final lexerElement =
  (labeledLexerElement * ebnfSuffix.optional)
  | (lexerAtom * ebnfSuffix.optional)
  | (lexerBlock * ebnfSuffix.optional)
  | (actionBlock * QUESTION.optional);

final labeledLexerElement = identifier * (ASSIGN | PLUS_ASSIGN) * (lexerAtom | lexerBlock);

final lexerBlock = LPAREN * reference('lexerAltList') * RPAREN;

final lexerCommands = RARROW * lexerCommand * (COMMA * lexerCommand).multiple.optional;

final lexerCommand =
  (lexerCommandName * LPAREN * lexerCommandExpr * RPAREN)
  | lexerCommandName;

final lexerCommandName = identifier | MODE;

final lexerCommandExpr = identifier | INT;

/* -= Rule Alts =- */

final altList = alternative * (OR * alternative).multiple.optional;

final alternative = 
  (elementOptions.optional * element.multiple)
  | empty();

final element = 
  (labeledElement * (ebnfSuffix | empty()))
  | (atom * (ebnfSuffix | empty()))
  | ebnf
  | actionBlock * QUESTION.optional;

final labeledElement = identifier * (ASSIGN | PLUS_ASSIGN) * (atom | block);

/* -= EBNF and blocks =- */

final ebnf = block * blockSuffix.optional;

final blockSuffix = ebnfSuffix;

final ebnfSuffix = 
  (QUESTION * QUESTION.optional)
  | (STAR * QUESTION.optional)
  | (PLUS * QUESTION.optional);

final lexerAtom =
  characterRange
  | terminal
  | notSet
  | LEXER_CHAR_SET
  | (DOT * elementOptions.optional);

final atom =
  terminal
  | ruleref
  | notSet
  | (DOT * elementOptions.optional);

/* -= Inverted element set =- */

final notSet =
  (NOT * setElement)
  | (NOT * blockSet);

final blockSet = LPAREN * setElement * (OR * setElement).multiple.optional * RPAREN;

final setElement =
  (TOKEN_REF * elementOptions.optional)
  | (STRING_LITERAL * elementOptions.optional)
  | characterRange
  | LEXER_CHAR_SET;

/* -= Grammar Block =- */

final block = LPAREN * (optionsSpec.optional * ruleAction.multiple.optional * COLON).optional * reference('altList') * RPAREN;

/* -= Parser rule ref =- */

final ruleref = RULE_REF * argActionBlock.optional * elementOptions.optional;

/* -= Character Range =- */

final characterRange = STRING_LITERAL * RANGE * STRING_LITERAL;

final terminal =
  (TOKEN_REF * elementOptions.optional)
  | (STRING_LITERAL * elementOptions.optional);

final elementOptions = LT * elementOption * (COMMA * elementOption).multiple.optional * GT;

final elementOption =
  identifier
  | (identifier * ASSIGN * (identifier | STRING_LITERAL));

final identifier = RULE_REF | TOKEN_REF;