import 'dart:io';

import 'package:token_parser/token_parser.dart';
import 'package:token_parser/src/lexemes/global.dart';
import 'package:token_parser/debug.dart';

import 'package:antlr_parser/src/v4/basic.dart' as basic;
import 'package:antlr_parser/src/v4/lexer.dart' as lexer;
import 'package:antlr_parser/src/v4/parser.dart' as parser;

void main() {
  final input = File('./example/antlr/v4/Dart.g').readAsStringSync();
  print(antlr4.parse(input).get(lexeme: parser.lexerAtom));
}

// ignore: non_constant_identifier_names
final antlr4 = DebugGrammar(
  showPath: true,
  main: ~parser.grammarSpec,
  // remove: lexer.COMMENT,
  rules: {
    /* -= Parser Rules =- */
    'grammarSpec': parser.grammarSpec,
    'grammarDecl': parser.grammarDecl,
    'grammarType': parser.grammarType,
    'prequelConstruct': parser.prequelConstruct,
    'optionsSpec': parser.optionsSpec,
    'option': parser.option,
    'optionValue': parser.optionValue,
    'delegateGrammars': parser.delegateGrammars,
    'delegateGrammar': parser.delegateGrammar,
    'tokensSpec': parser.tokensSpec,
    'channelsSpec': parser.channelsSpec,
    'idList': parser.idList,
    'action_': parser.action_,
    'actionScopeName': parser.actionScopeName,
    'actionBlock': parser.actionBlock,
    'argActionBlock': parser.argActionBlock,
    'modeSpec': parser.modeSpec,
    'rules': parser.rules,
    'ruleSpec': parser.ruleSpec,
    'parserRuleSpec': parser.parserRuleSpec,
    'exceptionGroup': parser.exceptionGroup,
    'exceptionHandler': parser.exceptionHandler,
    'finallyClause': parser.finallyClause,
    'rulePrequel': parser.rulePrequel,
    'ruleReturns': parser.ruleReturns,
    'throwsSpec': parser.throwsSpec,
    'localsSpec': parser.localsSpec,
    'ruleAction': parser.ruleAction,
    'ruleModifiers': parser.ruleModifiers,
    'ruleModifier': parser.ruleModifier,
    'ruleBlock': parser.ruleBlock,
    'ruleAltList': parser.ruleAltList,
    'labeledAlt': parser.labeledAlt,
    'lexerRuleSpec': parser.lexerRuleSpec,
    'lexerRuleBlock': parser.lexerRuleBlock,
    'lexerAltList': parser.lexerAltList,
    'lexerAlt': parser.lexerAlt,
    'lexerElements': parser.lexerElements,
    'lexerElement': parser.lexerElement,
    'labeledLexerElement': parser.labeledLexerElement,
    'lexerBlock': parser.lexerBlock,
    'lexerCommands': parser.lexerCommands,
    'lexerCommand': parser.lexerCommand,
    'lexerCommandName': parser.lexerCommandName,
    'lexerCommandExpr': parser.lexerCommandExpr,
    'altList': parser.altList,
    'alternative': parser.alternative,
    'element': parser.element,
    'labeledElement': parser.labeledElement,
    'ebnf': parser.ebnf,
    'blockSuffix': parser.blockSuffix,
    'ebnfSuffix': parser.ebnfSuffix,
    'lexerAtom': parser.lexerAtom,
    'atom': parser.atom,
    'notSet': parser.notSet,
    'blockSet': parser.blockSet,
    'setElement': parser.setElement,
    'block': parser.block,
    'ruleref': parser.ruleref,
    'characterRange': parser.characterRange,
    'terminal': parser.terminal,
    'elementOptions': parser.elementOptions,
    'elementOption': parser.elementOption,
    'identifier': parser.identifier,

    /* -= Lexer Rules =- */
    'DOC_COMMENT': lexer.DOC_COMMENT,
    'BLOCK_COMMENT': lexer.BLOCK_COMMENT,
    'LINE_COMMENT': lexer.LINE_COMMENT,
    'INT': lexer.INT,
    'STRING_LITERAL': lexer.STRING_LITERAL,
    'UNTERMINATED_STRING_LITERAL': lexer.UNTERMINATED_STRING_LITERAL,
    'BEGIN_ARGUMENT': lexer.BEGIN_ARGUMENT,
    'BEGIN_ACTION': lexer.BEGIN_ACTION,
    'OPTIONS': lexer.OPTIONS,
    'TOKENS': lexer.TOKENS,
    'CHANNELS': lexer.CHANNELS,
    'WSNLCHARS': lexer.WSNLCHARS,
    'IMPORT': lexer.IMPORT,
    'FRAGMENT': lexer.FRAGMENT,
    'LEXER': lexer.LEXER,
    'PARSER': lexer.PARSER,
    'GRAMMAR': lexer.GRAMMAR,
    'PROTECTED': lexer.PROTECTED,
    'PUBLIC': lexer.PUBLIC,
    'PRIVATE': lexer.PRIVATE,
    'RETURNS': lexer.RETURNS,
    'LOCALS': lexer.LOCALS,
    'THROWS': lexer.THROWS,
    'CATCH': lexer.CATCH,
    'FINALLY': lexer.FINALLY,
    'MODE': lexer.MODE,
    'COLON': lexer.COLON,
    'COLONCOLON': lexer.COLONCOLON,
    'COMMA': lexer.COMMA,
    'SEMI': lexer.SEMI,
    'LPAREN': lexer.LPAREN,
    'RPAREN': lexer.RPAREN,
    'LBRACE': lexer.LBRACE,
    'RBRACE': lexer.RBRACE,
    'RARROW': lexer.RARROW,
    'LT': lexer.LT,
    'GT': lexer.GT,
    'ASSIGN': lexer.ASSIGN,
    'QUESTION': lexer.QUESTION,
    'STAR': lexer.STAR,
    'PLUS_ASSIGN': lexer.PLUS_ASSIGN,
    'PLUS': lexer.PLUS,
    'OR': lexer.OR,
    'DOLLAR': lexer.DOLLAR,
    'RANGE': lexer.RANGE,
    'DOT': lexer.DOT,
    'AT': lexer.AT,
    'POUND': lexer.POUND,
    'NOT': lexer.NOT,
    'ID': lexer.ID,
    'WS': lexer.WS,
    'ERRCHAR': lexer.ERRCHAR,
    'NESTED_ARGUMENT': lexer.NESTED_ARGUMENT,
    'ARGUMENT_ESCAPE': lexer.ARGUMENT_ESCAPE,
    'ARGUMENT_STRING_LITERAL': lexer.ARGUMENT_STRING_LITERAL,
    'ARGUMENT_CHAR_LIETERAL': lexer.ARGUMENT_CHAR_LIETERAL,
    'END_ARGUMENT': lexer.END_ARGUMENT,
    'UNTERMINATED_ARGUMENT': lexer.UNTERMINATED_ARGUMENT,
    'ARGUMENT_CONTENT': lexer.ARGUMENT_CONTENT,
    'NESTED_ACTION': lexer.NESTED_ACTION,
    'ACTION_ESCAPE': lexer.ACTION_ESCAPE,
    'ACTION_STRING_LITERAL': lexer.ACTION_STRING_LITERAL,
    'ACTION_CHAR_LIETERAL': lexer.ACTION_CHAR_LIETERAL,
    'ACTION_DOC_COMMENT': lexer.ACTION_DOC_COMMENT,
    'ACTION_BLOCK_COMMENT': lexer.ACTION_BLOCK_COMMENT,
    'ACTION_LINE_COMMENT': lexer.ACTION_LINE_COMMENT,
    'END_ACTION': lexer.END_ACTION,
    'UNTERMINATED_ACTION': lexer.UNTERMINATED_ACTION,
    'ACTION_CONTENT': lexer.ACTION_CONTENT,
    'LEXER_CHAR_SET_BODY': lexer.LEXER_CHAR_SET_BODY,
    'LEXER_CHAR_SET': lexer.LEXER_CHAR_SET,
    'UNTERMINATED_CHAR_SET': lexer.UNTERMINATED_CHAR_SET,
    'Id': lexer.Id,
    'TOKEN_REF': lexer.TOKEN_REF,
    'RULE_REF': lexer.RULE_REF,
  }
);