import { While } from './while';

export class DoWhile extends While {

  constructor(condition, commandBlock) {
    super(condition, commandBlock);
  }

  get testFirst () {
    return false;
  }
  
}