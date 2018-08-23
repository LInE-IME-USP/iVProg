export class Case {

  constructor (expression) {
    this.expression = expression;
    this.commands = [];
  }

  setCommands (commands) {
    this.commands = commands;
  }

  get isDefault () {
    return this.expression === null;
  }
}