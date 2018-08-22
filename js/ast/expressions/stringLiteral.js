import { Literal } from './literal';
export class StringLiteral extends Literal {
  
  constructor(value) {
    super('string');
    this.value = value;
  }
}