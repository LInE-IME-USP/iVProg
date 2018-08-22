import { Literal } from './literal';
export class VariableLiteral extends Literal {
  
  constructor(value) {
    super('variable');
    this.value = value;
  }
}