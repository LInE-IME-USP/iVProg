import { Output } from './output';
import $ from 'jquery';

export class DOMOutput extends Output {

  constructor (selector) {
    super();
    this.el = $(selector);
  }

  sendOutput (text) {
    text = text.replace("\n", '</br>');
    text = text.replace(/\t/g,'&#9;');
    const span = $('<span />').addClass('ivprog-io-output-text').html(text);
    this.el.append(span);
  }

  clear () {
    this.el.empty();
  }
}