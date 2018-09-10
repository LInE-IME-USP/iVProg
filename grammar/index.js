import * as PT from './pt/ivprog.g4';
import PTFuncs from './pt/langFunctions';
import * as EN from './en/ivprog.g4';
import ENFuncs from './en/langFunctions';
import * as ES from './es/ivprog.g4';
import ESFuncs from './es/langFunctions';

exports.pt = {lexer: PT.ivprog, langFuncs: PTFuncs};
exports.en = {lexer: EN.ivprog, langFuncs: ENFuncs};
exports.es = {lexer: ES.ivprog, langFuncs: ESFuncs};
