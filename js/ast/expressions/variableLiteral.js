import { Literal } from './literal';
import { Types } from './../types';

export class VariableLiteral extends Literal {
  
  constructor(id) {
    super(Types.UNDEFINED);
    this.id = id;
  }
}