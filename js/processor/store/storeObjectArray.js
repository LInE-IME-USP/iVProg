import { StoreObject } from './storeObject';

export class StoreObjectArray extends StoreObject {

  static get WRONG_LINE_NUMBER () {
    return 1;
  }

  static get WRONG_TYPE () {
    return 2;
  }

  static get WRONG_COLUMN_NUMBER () {
    return 3;
  }

  constructor (type, lines, columns, value = null, readOnly = false) {
    super(type, value, readOnly);
    this._lines = lines;
    this._columns = columns;
  }

  get lines () {
    return this._lines;
  }

  get columns () {
    return this._columns;
  }

  isCompatible (another) {
    if(another instanceof StoreObject) {
      if(((this.lines === -1 && another.lines > 0) ||
        (this.lines === another.lines))) {
          if ((this.columns === -1 && another.columns > 0) ||
            (this.columns === another.columns)) {
              return super.isCompatible(another);
          }
        }
    }
    return false;
  }

  get isVector () {
    return this.type.dimensions === 1;
  }

  get isValid () {
    if (this.value !== null) {
      if( this.isVector) {
        if(this.value.length !== this.lines) {
          return [StoreObjectArray.WRONG_LINE_NUMBER, this.value.length];;
        }
        const mustBeNull = this.value.find(v => !this.type.canAccept(v.type) );
        if(!!mustBeNull) {
          return [StoreObjectArray.WRONG_TYPE, this.value.indexOf(mustBeNull)];;
        }
      }
      return [];
    } else {
    if(this.lines !== this.value.length) {
      return [StoreObjectArray.WRONG_LINE_NUMBER, this.value.length];
    }
    for (let i = 0; i < this.lines; i++) {
      for (let j = 0; j < this.columns; j++) {
        const arr = this.value[i];
        if(arr.length !== this.columns) {
          return [StoreObjectArray.WRONG_COLUMN_NUMBER, arr.length];
        }
        const mustBeNull = arr.find(v => !this.type.canAccept(v.type) );
        if(!!mustBeNull) {
          return [StoreObjectArray.WRONG_TYPE, i, arr.indexOf(mustBeNull)];
        }            
      }
    }
      return [];
    }
  }
}
