import {Input} from './input';

export class DOMInput extends Input{

  constructor (element) {
    super();
    this.el = $(element);
    this.listeners = [];
    this.setupEvents();
  }

  setupEvents () {
    this.el.on('keydown', (e) => {
      const code = e.keyCode || e.which;
      if (code === 13) {
        let text = this.el.val();
        text = text.replace('[\n\r]+', '');
        this.notifyInput(text);
        this.el.val('');
      }
    });
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