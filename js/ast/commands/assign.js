import { Command } from './command';

export class Assign extends Command {
  
  constructor (id, expression) {
    super();
    this.id = id;
    this.expression = expression;
  }
}