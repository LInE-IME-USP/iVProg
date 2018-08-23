import { Literal } from './literal';
import {Types} from './../types';
export class BoolLiteral extends Literal {
  
  constructor(value) {
    super(Types.BOOLEAN);
    this.value = value;
  }
}