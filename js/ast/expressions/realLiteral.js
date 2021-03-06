import { Literal } from './literal';
import { Types } from './../../typeSystem/types';
import { convertToString } from './../../typeSystem/parsers';

export class RealLiteral extends Literal {
  
  constructor(value) {
    super(Types.REAL);
    this.value = value;
  }

  toString() {
    return convertToString(this.value, this.type);
  }
}