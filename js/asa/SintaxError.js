export class SintaxError extends Error {

  static createError (msg, token) {
    const val = SintaxError.getErrorString(msg, token);
    return new SintaxError(val);
  }

  static getErrorString (symbol, token) {
    return `Sintax error! Expecting '${symbol}' but found ${token.text} at line:${token.line}, column:${token.column}`;
  }

  constructor (...msg) {
    super(...msg);
    Error.captureStackTrace(this, SintaxError);
  }
}