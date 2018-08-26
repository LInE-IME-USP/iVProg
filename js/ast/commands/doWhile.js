import { While } from './while';

export class DoWhile extends While {

  constructor(expression, commandBlock) {
    super(expression, commandBlock);
  }

  get testFirst () {
    return false;
  }
  
}