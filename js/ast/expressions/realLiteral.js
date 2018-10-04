import { Literal } from './literal';
import { Types } from './../../typeSystem/types';

export class RealLiteral extends Literal {
  
  constructor(value) {
    super(Types.REAL);
    this.value = value;
  }
}