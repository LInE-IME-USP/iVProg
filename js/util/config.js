let config = null;

export class Config {

  static get config () {
    return config;
  }

  static setConfig(opts) {
    config = Object.assign(config, opts);
  }

  constructor (loopTimeout, decimalPlaces, intConvertRoundMode) {
    this.loopTimeout = loopTimeout;
    this.decimalPlaces = decimalPlaces;
    this.intConvertRoundMode = intConvertRoundMode;
  }
}

config = new Config(5000, 5, 2);