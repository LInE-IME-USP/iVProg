import { Declaration } from './declaration';

export class ArrayDeclaration extends Declaration {

  constructor (id, subtype, lines, columns, initial, isConst)   {
    super(id, 'array', initial, isConst);
    this.subtype = subtype;
    this.lines = lines;
    this.columns = columns;
  }
}