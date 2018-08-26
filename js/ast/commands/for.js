export class For {
  constructor (assignment, condition, increment, commandBlock) {
    this.assignment = assignment;
    this.condition = condition;
    this.increment = increment;
    this.commandBlock = commandBlock;
  }

  get commands () {
    return this.commandBlock.commands;
  }
}