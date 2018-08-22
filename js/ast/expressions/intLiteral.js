import { Literal } from './literal';
export class IntLiteral extends Literal {
  
  constructor(value) {
    super('int');
    this.value = value;
  }
}