import { Command } from './command';

export class For extends Command {

  constructor (assignment, condition, increment, commandBlock) {
    super();
    this.assignment = assignment;
    this.condition = condition;
    this.increment = increment;
    this.commandBlock = commandBlock;
  }

  get commands () {
    return this.commandBlock.commands;
  }
}