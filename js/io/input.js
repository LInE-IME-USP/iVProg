export class Input {

  registerListener (listener) {
    throw new Error("Must be implemented");
  }

  notifyInput (text) {
    throw new Error("Must be implemented");
  }

  requestInput () {
    throw new Error("Must be implemented");
  }

  removeListener (listener) {
    throw new Error("Must be implemented");
  }
}