import {Input} from './input';

export class DOMInput extends Input{

  constructor (element) {
    super();
    let id = element
    if(element[0] == '#') {
      id = element.substring(1);
    }
    this.el = document.getElementById(id);
    this.listeners = [];
    this.setupEvents();
  }

  setupEvents () {
    this.el.addEventListener('keydown', this.captureInput.bind(this));
  }

  captureInput (event) {
    const code = event.keyCode || event.which;
    if (code === 13) {
      let text = this.el.value;
      text = text.replace('[\n\r]+', '');
      this.notifyInput(text);
      this.el.value = "";
    }
  }

  requestInput (callback) {
    this.listeners.push(callback);
    this.el.focus();
  }

  notifyInput (text) {
    this.listeners.forEach(resolve => {
      resolve(text);
    })
    this.listeners.splice(0, this.listeners.length);
  }

}