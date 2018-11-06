import { Literal } from './literal';
import { Types } from './../../typeSystem/types';

export class IntLiteral extends Literal {
  
  constructor(value) {
    super(Types.INTEGER);
    this.value = value;
  }
}