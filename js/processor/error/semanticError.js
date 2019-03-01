export class SemanticError extends Error {

  constructor (...msg) {
    super(...msg);
    if(Error.captureStackTrace)
      Error.captureStackTrace(this, SemanticError);
  }
}