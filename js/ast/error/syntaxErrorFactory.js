import { LocalizedStrings } from './../../services/localizedStringsService';
import { SyntaxError } from './syntaxError';

export const SyntaxErrorFactory = Object.freeze({
  extra_lines: () => new SyntaxError(LocalizedStrings.getError("extra_lines")),
  token_missing_one: (expected, token) => {
    const context = [expected, token.text, token.line, token.column];
    return new SyntaxError(LocalizedStrings.getError("token_missing_one", context));
  },
  token_missing_list: (expectedList, token) => {
    const line = expectedList.join(LocalizedStrings.getOR());
    return SyntaxErrorFactory.token_missing_one(line, token);
  },
  id_missing: (token) => {
    const context = [token.text, token.line, token.column];
    return new SyntaxError(LocalizedStrings.getError("id_missing", context));
  },
  eos_missing: (token) => {
    const context = [token.line, token.column];
    return new SyntaxError(LocalizedStrings.getError("eos_missing", context));
  },
  invalid_array_dimension: (typeName, token) => {
    const context = [token.line, token.column, typeName];
    return new SyntaxError(LocalizedStrings.getError("invalid_array_dimension", context));
  },
  invalid_array_size: (token) => {
    const context = [token.line];
    return new SyntaxError(LocalizedStrings.getError("invalid_array_size", context));
  },
  invalid_main_return: (name, typeName, token) => {
    const context = [name, typeName, token.line];
    return new SyntaxError(LocalizedStrings.getError("invalid_main_return", context));
  },
  invalid_var_declaration: (token) => {
    const context = [token.line];
    return new SyntaxError(LocalizedStrings.getError("invalid_var_declaration", context));
  },
  invalid_break_command: (cmdName, token) => {
    const context = [token.line, cmdName];
    return new SyntaxError(LocalizedStrings.getError("invalid_break_command", context));
  },
  invalid_terminal: () => {
    const context = [token.text, token.line, token.column];
    return new SyntaxError(LocalizedStrings.getError('invalid_terminal', context));
  },
  invalid_type: (list, token) => {
    const line = list.join(LocalizedStrings.getOR());
    const context = [token.text, token.line, token.column, line]
    return new SyntaxError(LocalizedStrings.getError("invalid_type", context));
  }
});
