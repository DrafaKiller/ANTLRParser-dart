import 'dart:io';

import 'package:antlr_parser/antlr_parser.dart';

void main() {
  final example = File('./example/antlr/v4/Hello.g4').readAsStringSync();

  final result = antlr4.parse(example);
  print(result);
}
