import { Literal } from './literal';
export class RealLiteral extends Literal {
  
  constructor(value) {
    super('real');
    this.value = value;
  }
}