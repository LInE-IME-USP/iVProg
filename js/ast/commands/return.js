import { Command } from './command';

export class Return extends Command {

  constructor(expression) {
    super();
    this.expression = expression;
  }
  
}