import { Command } from './command';

export class Case extends Command {

  constructor (expression) {
    super();
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