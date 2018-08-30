import { Output } from './output';
import $ from 'jquery';

export class DOMOutput extends Output {

  constructor (selector) {
    super();
    this.el = $(selector);
  }

  sendOutput (text) {
    const line = $(`<span class='ivprog-io-output> ${text} </span>`);
    this.el.append(line);
  }
}