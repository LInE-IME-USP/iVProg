class ConfigObject {

  constructor () {
    this.loopTimeout = 5000;
    this.decimalPlaces = 5;
    this.intConvertRoundMode = 2;
    this.default_lang = 'pt';
  }

  setConfig (opts) {
    for (const key in opts) {
      if(this.hasOwnProperty(key)){
        this[key] = opts[key];
      }
    }
  }
}
let config = new ConfigObject();
export const Config = config;