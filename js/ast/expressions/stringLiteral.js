import { Literal } from './literal';
import {Types} from './../types';

export class StringLiteral extends Literal {
  
  constructor(value) {
    super(Types.STRING);
    this.value = value;
  }
}