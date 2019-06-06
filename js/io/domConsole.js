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

  static get INPUT () {
    return 3;
  }

  constructor (elementID) {
    this.input = null;
    this.cursorInterval = null;
    this.inputDiv = null;
    this.inputCMD = null;
    this.inputSpan = null;
    this.cursorRef = null;
    this.needInput = false;
    this.termDiv = null;
    this.anyKey = false;
    this.parent = $(elementID);
    this.setup();
    this.inputListeners = [];
    this.hideInput();
  }

  setup () {
    this._setupDom();
    this._setupEvents();
  }

  _setupEvents () {
    this.input.on("keydown", (event) => {
      if (!this.needInput) {
        return;
      }
      const keyCode = event.which;
      if (keyCode === 13 || this.anyKey) {
        let text = this.input.val();
        text = text.replace('[\n\r]+', '');
        this.notifyListeners(text);
        this._appendUserInput(text);
        this.input.val("");
        this.inputSpan.text("");
      }
    });
  }

  _setupDom () {
    this.termDiv = $("<div></div>");
    this.termDiv.addClass("ivprog-term-div");
    this.inputDiv = $(`
      <div id="ivprog-terminal-inputdiv">
        <div id="cmd">
          <span></span>
          <div id="cursor"></div>
        </div>
      </div>
    `);
    this.input = $('<input type="text" name="command" value=""/>');
    this.inputDiv.append(this.input);
    this.cursorRef = $(this.inputDiv.find("#cursor")[0]);
    this.inputSpan = $(this.inputDiv.find("#cmd").children('span')[0]);
    this.inputCMD = $(this.inputDiv.find("#cmd")[0]);
    //this.input.addClass("ivprog-term-input");
    this.termDiv.append(this.inputDiv);
    this.parent.append(this.termDiv);
    this._setupCursor();
  }

  _setupCursor () {
    const outerRef = this
    this.inputCMD.click(function() {
      if(outerRef.cursorInterval != null) {
        return;
      }
      outerRef.input.focus();
      outerRef.cursorInterval = window.setInterval(function() {
        if (outerRef.cursorRef.css('visibility') === 'visible') {
          outerRef.cursorRef.css({visibility: 'hidden'});
        } else {
          outerRef.cursorRef.css({visibility: 'visible'});
        }
      }, 550);
    });

    this.inputCMD.click();
    
    this.input.keyup(function() {
      outerRef.inputSpan.text(outerRef.input.val());
    });

    this.input.blur(function() {
      clearInterval(outerRef.cursorInterval);
      outerRef.cursorInterval = null;
      outerRef.cursorRef.css({visibility: 'visible'});
    });
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
    textDiv.insertBefore(this.inputDiv);
    this.scrollTerm();
  }

  _appendUserInput (text) {
    const divClass = this.getClassForType(DOMConsole.INPUT);
    const textDiv = $(`<div>
      <i class="icon keyboard outline" style="float:left"></i>
      <span>${text}</span>
    </div>`);
    textDiv.addClass(divClass);
    textDiv.insertBefore(this.inputDiv);
    this.scrollTerm();
  }

  scrollTerm () {
    this.termDiv.animate({
      scrollTop: this.termDiv.prop('scrollHeight')
    }, 0);
  }

  focus () {
    this.parent.show();
    const prev = this.inputDiv.prev();
    if(prev.length > 0)
      prev[0].scrollIntoView();
  }

  hide () {
    this.parent.hide();
  }

  getClassForType (type) {
    switch (type) {
      case DOMConsole.INPUT:
        return "ivprog-term-userInput";
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
    if(this.cursorInterval != null) {
      clearInterval(this.cursorInterval);
    }
    this.inputCMD.off();
  }

  showInput () {
    this.needInput = true;
    this.inputDiv.show();
    this.inputCMD.click();
    this.inputCMD[0].scrollIntoView();
  }

  hideInput () {
    this.needInput = false;
    this.inputDiv.hide();
    clearInterval(this.cursorInterval);
    this.cursorInterval = null;
  }

  requestInput (callback, anyKey = false) {
    this.inputListeners.push(callback);
    this.anyKey = anyKey;
    this.showInput();
  }

  sendOutput (text) {
    const output = ""+text;
    output.split("\n").forEach(t => {
      t = t.replace(/\t/g,'&#9;');
      this.write(t)
    });
  }

  clear () {
    this.inputDiv.parent().children().not(this.inputDiv).remove();
    this.input.val("");
    this.inputSpan.text("");
  }
}