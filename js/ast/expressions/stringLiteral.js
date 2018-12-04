import { Literal } from './literal';
import { Types } from './../../typeSystem/types';

export class StringLiteral extends Literal {
  
  constructor(value) {
    super(Types.STRING);
    this.value = value;
  }

  toString() {
    return this.value;
  }
}