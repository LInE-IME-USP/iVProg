export class SyntaxError extends Error {

  constructor (...msg) {
    super(...msg);
    if(Error.captureStackTrace)
      Error.captureStackTrace(this, SyntaxError);
  }
}
