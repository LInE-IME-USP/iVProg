export class For {
  constructor (attribution, condition, increment, commandBlock) {
    this.attribution = attribution;
    this.condition = condition;
    this.increment = increment;
    this.commandBlock = commandBlock;
  }

  get commands () {
    return this.commandBlock.commands;
  }
}