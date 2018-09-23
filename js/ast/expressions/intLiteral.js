import { Literal } from './literal';
import {Types} from './../types';

export class IntLiteral extends Literal {
  
  constructor(value) {
    super(Types.INTEGER);
    this.value = value;
  }
}