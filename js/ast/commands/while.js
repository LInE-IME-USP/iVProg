export class While {

  constructor (expression, commandBlock) {
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