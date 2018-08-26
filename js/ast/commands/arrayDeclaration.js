import { Declaration } from './declaration';
import {Types} from './../types';
export class ArrayDeclaration extends Declaration {

  constructor (id, subtype, lines, columns, initial, isConst)   {
    super(id, Types.ARRAY, initial, isConst);
    this.subtype = subtype;
    this.lines = lines;
    this.columns = columns;
  }
}