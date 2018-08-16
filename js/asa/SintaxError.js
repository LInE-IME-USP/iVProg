export class SintaxError extends Error{
  constructor(...msg) {
    super(...msg);
    Error.captureStackTrace(this, SintaxError);
  }
}