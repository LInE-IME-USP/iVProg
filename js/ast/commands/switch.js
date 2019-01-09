import { Command } from './command';

export class Switch extends Command {
  
  constructor (expression, cases) {
    super();
    this.expression = expression;
    this.cases = cases;
  }
}