class ConfigObject {

  constructor () {
    this.loopTimeout = 5000;
    this.decimalPlaces = 8;
    this.intConvertRoundMode = 2;
    this.default_lang = 'pt';
    this.enable_type_casting = true;
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