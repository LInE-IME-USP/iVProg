import { Assign } from './assign';

export class ArrayIndexAssign extends Assign {

  constructor (id, lineExpression, columnExpression, expression) {
    super(id, expression);
    this.line = lineExpression;
    this.column = columnExpression;
  }
}
