import { Expression } from './expression';

export class Literal extends Expression {
  
  constructor (type) {
    super();
    this.type = type;
  }
}