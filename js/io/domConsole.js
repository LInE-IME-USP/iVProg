import { LocalizedStrings } from "./../services/localizedStringsService";
import { Config } from "./../util/config";

export class DOMConsole {

  static get BASH_TEMPLATE () {
    return `
    <div class="bash-title">
      <i id="ivprog-console-clearbtn" class="icon eraser" style="float:left;padding-left: 5px"></i>
      <span>Terminal</span>
      <i id="ivprog-console-showbtn" class="icon window maximize outline" style="float:right"></i>
      <i id="ivprog-console-hidebtn" class="icon window minimize outline" style="float:right"></i>
    </div>
    <div id='ivprog-term' class="bash-body"></div>`;
  }

  static get INPUT_CARET_TEMPLATE () {
    return `
    <div id="cmd">
      <span></span>
      <div id="cursor"></div>
    </div>`;
  }

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
    this.idleInterval = null;
    this.inputDiv = null;
    this.inputCMD = null;
    this.inputSpan = null;
    this.cursorRef = null;
    this.needInput = false;
    this.clearBtn = null;
    this.hideBtn = null;
    this.showBtn = null;
    this.termDiv = null;
    this.anyKey = false;
    let actualID = elementID
    if (elementID[0] === '#') {
      actualID = elementID.substring(1);
    }
    this.parent = document.getElementById(actualID)
    this.setup();
    this.inputListeners = [];
    this.hideInput();
  }

  setup () {
    this._setupDom();
    this._setupEvents();
  }

  _setupEvents () {
    this.input.addEventListener('keydown', this.registerInput.bind(this));
    this.clearBtn.addEventListener('click', this.clearBtnClick.bind(this));
    this.hideBtn.addEventListener('click', this.hideBtnClick.bind(this));
    this.showBtn.addEventListener('click', this.showBtnClick.bind(this));
  }

  registerInput (event) {
    if (!this.needInput) {
      return;
    }
    const keyCode = event.which;
    if (keyCode === 13 || this.anyKey) {
      if(this.idleInterval != null) {
        clearInterval(this.idleInterval);
        this.idleInterval = null;
      }
      let text = this.input.value;
      text = text.replace('[\n\r]+', '');
      this.notifyListeners(text);
      this._appendUserInput(text);
      this.input.value = '';
      this.inputSpan.innerHTML = '';
    }
  }

  _setupDom () {
    const bashNode = document.createElement('div');
    bashNode.classList.add('bash');
    bashNode.innerHTML = DOMConsole.BASH_TEMPLATE;
    this.termDiv = bashNode.querySelector("#ivprog-term");
    this.termDiv.classList.add("ivprog-term-div");
    this.inputDiv = document.createElement("div");
    this.inputDiv.id = "ivprog-terminal-inputdiv";
    this.inputDiv.innerHTML = DOMConsole.INPUT_CARET_TEMPLATE;
    this.input = document.createElement("input");
    this.input.setAttribute("name", "command");
    this.input.setAttribute("value", "");
    this.input.setAttribute("type", "text");
    this.inputDiv.append(this.input);
    this.termDiv.append(this.inputDiv);
    bashNode.append(this.termDiv);
    this.parent.append(bashNode);
    this.inputCMD = this.inputDiv.querySelector("#cmd");
    this.cursorRef = this.inputCMD.querySelector("#cursor");
    this.inputSpan = this.inputCMD.querySelector('span');
    this.clearBtn = bashNode.querySelector('#ivprog-console-clearbtn');
    this.hideBtn = bashNode.querySelector('#ivprog-console-hidebtn');
    this.showBtn = bashNode.querySelector('#ivprog-console-showbtn');
    this._setupCursor();
    //Jquery tooltips....
    $(this.clearBtn).popup({content:LocalizedStrings.getUI("terminal_clear")});
    $(this.showBtn).popup({content:LocalizedStrings.getUI("terminal_show")});
    $(this.hideBtn).popup({content:LocalizedStrings.getUI("terminal_hide")});
  }

  _setupCursor () {
    this.inputCMD.addEventListener('click', this.blinkCaretAndFocus.bind(this));
    this.inputCMD.click();
    
    this.input.addEventListener('keyup', this.updateSpanText.bind(this));
    this.input.addEventListener('blur', this.stopBlinkCaret.bind(this));
  }

  blinkCaretAndFocus () {
    if(this.cursorInterval != null) {
      return;
    }
    this.input.focus();
    const outerRef = this;
    this.cursorInterval = window.setInterval(function() {
      if (outerRef.cursorRef.style.visibility === 'visible') {
        outerRef.cursorRef.style.visibility = 'hidden';
      } else {
        outerRef.cursorRef.style.visibility = 'visible';
      }
    }, 500);
  }

  updateSpanText () {
    this.inputSpan.innerHTML = this.input.value;
    if(this.idleInterval != null)
      window.clearInterval(this.idleInterval);
    this.scheduleNotify()
  }

  stopBlinkCaret () {
    clearInterval(this.cursorInterval);
    this.cursorInterval = null;
    this.cursorRef.style.visibility = 'visible';
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
    const textDiv = document.createElement('div');
    textDiv.classList.add(divClass);
    textDiv.innerHTML = this.getOutputText(text);
    this.termDiv.insertBefore(textDiv, this.inputDiv);
    this.scrollTerm();
  }

  _appendUserInput (text) {
    const divClass = this.getClassForType(DOMConsole.INPUT);
    const textDiv = document.createElement('div');
    textDiv.innerHTML = this.getUserInputText(text);
    textDiv.classList.add(divClass);
    this.termDiv.insertBefore(textDiv, this.inputDiv);
    this.scrollTerm();
  }

  getOutputText (text) {
    return `<span>${text}</span>`;
  }

  getUserInputText (text) {
    return `<i class="icon keyboard outline" style="float:left"></i><span>${text}</span>`;
  }

  scrollTerm () {
    //scrollIt(this.inputDiv.previousSibling,200);
    this.inputDiv.previousSibling.scrollIntoView(false);
  }

  focus () {
    this.termDiv.style.display = 'block';
    // Is in draggable mode?
    console.log(this.parent.style.top.length);
    if(this.parent.style.top.length == 0) {
      this.parent.style.marginTop = "-160px";
    }
    const prev = this.inputDiv.closest('div');
    if(prev != null)
      prev.scrollIntoView(false);
  }

  hide () {
    // Is in draggable mode?
    if(this.parent.style.top.length == 0) {
      this.parent.style.marginTop = "0";
    }
    this.termDiv.style.display = 'none';
    
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
    this.input.removeEventListener('keyup', this.updateSpanText.bind(this));
    this.input.removeEventListener('blur', this.stopBlinkCaret.bind(this));
    this.input.removeEventListener('keydown', this.registerInput.bind(this));
    this.inputCMD.removeEventListener('click', this.blinkCaretAndFocus.bind(this));
    this.clearBtn.removeEventListener('click', this.clearBtnClick.bind(this));
    this.hideBtn.removeEventListener('click', this.hideBtnClick.bind(this));
    this.showBtn.removeEventListener('click', this.showBtnClick.bind(this));
    this.input = null;
    this.inputCMD =  null;
    this.inputDiv = null;
    this.termDiv = null;
    this.inputSpan = null;
    this.cursorRef = null;
    this.clearBtn = null;
    this.hideBtn = null;
    this.showBtn = null;
    const cNode = this.parent.cloneNode(false);
    this.parent.parentNode.replaceChild(cNode, this.parent);
    if(this.cursorInterval != null) {
      clearInterval(this.cursorInterval);
    }
    if(this.idleInterval != null) {
      clearInterval(this.idleInterval);
    }
  }

  showInput () {
    this.needInput = true;
    this.inputDiv.style.display = 'block';
    this.inputCMD.click();
    this.inputCMD.scrollIntoView();
  }

  hideInput () {
    this.needInput = false;
    this.inputDiv.style.display = ' none';
    clearInterval(this.cursorInterval);
    this.cursorInterval = null;
  }

  requestInput (callback, anyKey = false) {
    this.inputListeners.push(callback);
    this.anyKey = anyKey;
    if(this.idleInterval == null)
      this.scheduleNotify();
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
    while(this.inputDiv.parentElement.childNodes.length > 1) {
      this.inputDiv.parentElement.removeChild(this.inputDiv.parentElement.firstChild);
    }
    this.input.value = "";
    this.inputSpan.innerHTML = '';
  }

  clearBtnClick () {
    this.clear();
  }

  showBtnClick () {
    this.focus();
  }

  hideBtnClick () {
    this.hide();
  }

  notifyIdle () {
    this.info(LocalizedStrings.getMessage('awaiting_input_message'));
    this.inputCMD.click();
  }
  
  scheduleNotify () {
    this.idleInterval = window.setInterval(this.notifyIdle.bind(this), Config.idle_input_interval);
  }
}