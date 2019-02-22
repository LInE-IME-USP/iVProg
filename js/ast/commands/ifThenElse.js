import { Command } from './command';

export class IfThenElse extends Command {

  constructor (condition, ifTrue, ifFalse) {
    super();
    this.condition = condition;
    this.ifTrue = ifTrue;
    this.ifFalse = ifFalse;
  }
}