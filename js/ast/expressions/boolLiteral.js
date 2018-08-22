import { Literal } from './literal';
export class BoolLiteral extends Literal {
  
  constructor(value) {
    super('bool');
    this.value = value;
  }
}