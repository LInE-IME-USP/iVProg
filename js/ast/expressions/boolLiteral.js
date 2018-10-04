import { Literal } from './literal';
import { Types } from './../../typeSystem/types';

export class BoolLiteral extends Literal {
  
  constructor(value) {
    super(Types.BOOLEAN);
    this.value = value;
  }
}