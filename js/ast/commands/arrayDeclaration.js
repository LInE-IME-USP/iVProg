import { Declaration } from './declaration';

export class ArrayDeclaration extends Declaration {

  constructor (id, type, lines, columns, initial, isConst)   {
    super(id, type, initial, isConst);
    this.lines = lines;
    this.columns = columns;
  }
}