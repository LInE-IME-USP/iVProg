import { Command } from './command';

export class Declaration extends Command {
  
  constructor (id, type, initial, isConst) {
    super();
    this.id = id;
    this.type = type;
    this.initial = initial;
    this.isConst = isConst;
  }
}
