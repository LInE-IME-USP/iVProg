import { Output } from './output';

export class DOMOutput extends Output {

  constructor (selector) {
    super();
    let id = selector;
    if (selector[0] == '#') {
      id = selector.substring(1);
    }
    this.el = document.getElementById(id);
  }

  sendOutput (text) {
    text = text.replace("\n", '</br>');
    text = text.replace(/\t/g,'&#9;');
    const span = document.createElement('span');
    span.classList.add('ivprog-io-output-text');
    span.innerHTML = text;
    this.el.append(span);
  }

  clear () {
    while(this.el.childNodes.length > 0) {
      this.el.removeChild(this.el.firstChild);
    }
  }
}