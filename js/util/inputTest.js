import { Input } from './../io/input';
import { LocalizedStrings } from '../services/localizedStringsService';

export class InputTest extends Input {

  constructor (inputList) {
    super();
    this.index = 0;
    this.inputList = inputList;
  }

  requestInput (callback) {
    if(this.index < this.inputList.length) {      
      callback(this.inputList[this.index]);
      this.index++;
    } else {
      throw new Error(LocalizedStrings.getError("exceeded_input_request"));
    }
  }
}