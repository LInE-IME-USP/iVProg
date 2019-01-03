import { Command } from './command';

export class While extends Command {

  constructor (expression, commandBlock) {
    super();
    this.expression = expression;
    this.commandBlock = commandBlock;
  }

  get commands () {
    return this.commandBlock.commands;
  }

  get testFirst () {
  	return true;
  }
}