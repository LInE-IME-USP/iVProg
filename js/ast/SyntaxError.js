export class SyntaxError extends Error {

  static createError (msg, token) {
    const val = SyntaxError.getErrorString(msg, token);
    return new SyntaxError(val);
  }

  static getErrorString (symbol, token) {
    return `Syntax error: Expecting '${symbol}' but found '${token.text}' at line:${token.line}, column:${token.column}`;
  }

  constructor (...msg) {
    super(...msg);
    Error.captureStackTrace(this, SyntaxError);
  }
}