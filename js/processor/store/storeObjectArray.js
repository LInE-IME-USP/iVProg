import { Types } from './../../ast/types';
import { StoreObject } from './storeObject';

export class StoreObjectArray extends StoreObject {

  constructor (subtype, lines, columns, value = null, readOnly = false) {
    super(Types.ARRAY, value, readOnly);
    this._lines = lines;
    this._columns = columns;
    this._subtype = subtype;
  }

  get lines () {
    return this._lines;
  }

  get columns () {
    return this._columns;
  }

  get subtype () {
    return this._subtype;
  }

  isCompatible (another) {
    if(another instanceof StoreObject) {
      if(this.lines === another.lines &&
        this.columns === another.columns &&
        this.subtype === another.subtype) {
          return super.isCompatible(another);
        }
    }
    return false;
  }

  get isVector () {
    return this.columns === null;
  }

  get isValid () {
    if (this.value !== null) {
      if( this.isVector) {
        if(this.value.length !== this.lines) {
          return false;
        }
        const mustBeNull = this.value.find(v => v.type !== this.subtype);
        if(!!mustBeNull) {
          return false;
        }
      }
      return true;
    } else {
    if(this.lines !== this.value.length) {
      return false;
    }
    for (let i = 0; i < this.lines; i++) {
      for (let j = 0; j < this.columns; j++) {
        const arr = this.value[i];
        if(arr.length !== this.columns) {
          return false;
        }
        const mustBeNull = arr.find(v => v.type !== this.subtype);
        if(!!mustBeNull) {
          return false;
        }            
      }
    }
      return true;
    }
  }
}
