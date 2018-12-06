import { Literal } from './literal';
import { Types } from './../../typeSystem/types';
import { convertToString } from './../../typeSystem/parsers';

export class IntLiteral extends Literal {
  
  constructor(value) {
    super(Types.INTEGER);
    this.value = value;
  }

  toString() {
    return convertToString(this.value, this.type);
  }
}