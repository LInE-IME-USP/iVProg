import { Expression } from './expression';

export class InfixApp extends Expression {

  constructor(op, left, right) {
    super();
    this.op = op;
    this.left = left;
    this.right = right;
  }
}