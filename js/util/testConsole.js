import { DOMConsole } from "../io/domConsole";
import { getInstance } from "../services/localizedStringsService";

const LocalizedStrings = getInstance();

export class TestConsole {


  constructor (inputList) {
    this.index = 0;
    this.inputList = inputList;
    this.list = [];
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

  _appendText (text) {
    this.list.push(text);
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

  requestInput (callback) {
    if(this.index < this.inputList.length) {      
      callback(this.inputList[this.index]);
      this.index++;
    } else {
      throw new Error(LocalizedStrings.getError("exceeded_input_request"));
    }
  }

  sendOutput (text) {
    const output = ""+text;
    output.split("\n").forEach(t => {
      t = t.replace(/\t/g,'&#9;');
      this.write(t)
    });
  }
}