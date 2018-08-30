import { Output } from './../io/output';

export class OutputTest extends Output {

  constructor () {
    super();
    this.list = [];
  }

  sendOutput (text) {
    this.list.push(text);
  }
}