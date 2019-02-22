/**
 * This class represents all the language defined functions.
 * The language processor uses the id provided here to properly execute the desired function.
 * The function is actually implemented inside the language processor.
 * All the functions can be found at: js/processor/definedFunctions.js
 */
export class SysCall {

  constructor (langFunc) {
    this.langFunc = langFunc;
  }
}