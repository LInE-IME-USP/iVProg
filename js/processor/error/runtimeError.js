export class RuntimeError extends Error {

  constructor (...msg) {
    super(...msg);
    if(Error.captureStackTrace)
      Error.captureStackTrace(this, RuntimeError);
  }
}