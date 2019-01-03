import * as PT from './pt/ivprog.g4';
import PTFuncs from './pt/langFunctions';
import PTLibs from './pt/langLibs';
import * as EN from './en/ivprog.g4';
import ENFuncs from './en/langFunctions';
import ENLibs from './en/langLibs';
import * as ES from './es/ivprog.g4';
import ESFuncs from './es/langFunctions';
import ESLibs from './es/langLibs';

exports.pt = {lexer: PT.ivprog, langFuncs: PTFuncs, langLibs: PTLibs};
exports.en = {lexer: EN.ivprog, langFuncs: ENFuncs, langLibs: ENLibs};
exports.es = {lexer: ES.ivprog, langFuncs: ESFuncs, langLibs: ESLibs};
