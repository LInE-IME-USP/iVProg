import {Input} from './input';
import $ from 'jquery';

export class DOMInput extends Input{

  constructor (element) {
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

  registerListener (listener) {
    if(!listener.notify) {
      throw new Error("InternalError: Input listener must implement a notify function.");
    }
    this.listeners.push(listener);
  }

  requestInput (callback) {
    this.listeners.push(callback);
    this.el.focus();
  }

  removeListener (listener) {
    const idx = this.listeners.indexOf(listener);
    if (idx)
      this.listeners.splice(idx, 1);
  }

  notifyInput (text) {
    this.listeners.forEach(resolve => {
      resolve(l);
    })
    this.listeners.splice(0, this.listeners.length);
  }

}