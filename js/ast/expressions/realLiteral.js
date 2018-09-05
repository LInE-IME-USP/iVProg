import { Literal } from './literal';
import {Types} from './../types';

export class RealLiteral extends Literal {
  
  constructor(value) {
    super(Types.REAL);
    this.value = value;
  }
}