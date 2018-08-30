import { Input } from './../io/input';

export class InputTest extends Input {

  constructor (inputList) {
    super();
    this.inputList = inputList;
  }

  requestInput (callback) {
    if(this.inputList.length <= 0) {
      throw new Error('The amount of requests exceeded the amount of available inputs');
    } else {
      callback(this.inputList.splice(0,1)[0]);
    }
  }
}