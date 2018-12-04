import { Expression } from './expression';

export class InfixApp extends Expression {

  constructor(op, left, right) {
    super();
    this.op = op;
    this.left = left;
    this.right = right;
  }

  toString () {
    const l = this.left.toString();
    const op = this.op.value;
    const r = this.right.toString();
    return l + op + r;
  }
}