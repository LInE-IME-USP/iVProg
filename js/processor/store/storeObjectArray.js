import { StoreObject } from './storeObject';

export class StoreObjectArray extends StoreObject {

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
      if(this.lines === another.lines &&
        this.columns === another.columns) {
          return super.isCompatible(another);
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
          return false;
        }
        const mustBeNull = this.value.find(v => !this.type.canAccept(v.type) );
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
        const mustBeNull = arr.find(v => !this.type.canAccept(v.type) );
        if(!!mustBeNull) {
          return false;
        }            
      }
    }
      return true;
    }
  }
}
