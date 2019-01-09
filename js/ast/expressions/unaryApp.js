import {InfixApp} from './infixApp';

export class UnaryApp extends InfixApp {
  
  constructor (op, left) {
    super(op, left, null);
  }

  toString () {
    const l = this.left.toString();
    const op = this.op.value;
    return op + l;
  }
}