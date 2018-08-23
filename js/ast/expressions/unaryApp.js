import {InfixApp} from './infixApp';

export class UnaryApp extends InfixApp {
  
  constructor (op, left) {
    super(op, left, null);
  }
}