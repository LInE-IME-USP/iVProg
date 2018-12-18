import $ from 'jquery';

export class DOMConsole {

  static get USER () {
    return 0;
  }

  static get INFO () {
    return 1;
  }

  static get ERR () {
    return 2;
  }

  constructor (elementID) {
    this.input = null;
    this.needInput = false;
    this.termDiv = null;
    this.anyKey = false;
    this.parent = $(elementID);
    this.setup();
    this.inputListeners = [];
  }

  setup () {
    this._setupDom();
    this._setupEvents();
  }

  _setupEvents () {
    this.input.on("keydown", (event) => {
      if (!this.needInput) {
        event.preventDefault();
        return;
      }
      const keyCode = event.which;
      if (keyCode === 13 || this.anyKey) {
        let text = this.input.val();
        text = text.replace('[\n\r]+', '');
        this.notifyListeners(text);
        this.write(text);
        this.input.val("");
      }
    });
  }

  _setupDom () {
    this.termDiv = $("<div></div>");
    this.termDiv.addClass("ivprog-term-div");
    this.input = $('<input text="type">')
    this.input.addClass("ivprog-term-input");
    this.termDiv.append(this.input);
    this.parent.append(this.termDiv);
  }

  notifyListeners (text) {
    this.inputListeners.forEach(resolve => resolve(text));
    this.inputListeners.splice(0, this.inputListeners.length);
    this.hideInput();
    this.anyKey = false;
  }

  write (text) {
    this._appendText(text, DOMConsole.USER);
  }

  info (text) {
    this._appendText(text, DOMConsole.INFO);
  }

  err (text) {
    this._appendText(text, DOMConsole.ERR);
  }

  _appendText (text, type) {
    const divClass = this.getClassForType(type);
    const textDiv = $("<div></div>");
    textDiv.addClass(divClass);
    textDiv.append(text);
    textDiv.insertBefore(this.input);
    this.scrollTerm();
  }

  scrollTerm () {
    this.termDiv.animate({
      scrollTop: this.termDiv.prop('scrollHeight')
    }, 0);
  }

  getClassForType (type) {
    switch (type) {
      case DOMConsole.USER:
        return "ivprog-term-userText";
      case DOMConsole.INFO:
        return "ivprog-term-info";
      case DOMConsole.ERR:
        return "ivprog-term-error";
    }
  }

  dispose () {
    this.parent.off();
    this.input.off();
    this.input = null;
    this.parent.empty();
  }

  showInput () {
    this.needInput = true;
    this.input.show();
    this.input.focus();
  }

  hideInput () {
    this.needInput = false;
    this.input.hide();
  }

  requestInput (callback, anyKey = false) {
    this.inputListeners.push(callback);
    this.anyKey = anyKey;
    this.showInput();
  }

  sendOutput (text) {
    text.split("\n").forEach(t => {
      t = t.replace(/\t/g,'&#9;');
      this.write(t)
    });
  }

  clear () {
    this.input.parent().children().not(this.input).remove();
    this.input.val("");
  }
}