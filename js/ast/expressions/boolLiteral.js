import { Literal } from './literal';
import { Types } from './../../typeSystem/types';
import { convertBoolToString } from "./../../typeSystem/parsers";

export class BoolLiteral extends Literal {
  
  constructor(value) {
    super(Types.BOOLEAN);
    this.value = value;
  }

  toString () {
    return convertBoolToString(this.value);
  }
}