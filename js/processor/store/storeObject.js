import Decimal from 'decimal.js';

export class StoreObject {

  constructor (type, value, readOnly = false) {
    this._type = type;
    this._value = value;
    this._readOnly = readOnly;
    this._id = null;
  }

  setID (id) {
    this._id = id;
  }

  get id () {
    return this._id;
  }

  get inStore () {
    return this.id !== null;
  }

  get type () {
    return this._type;
  }

  get value () {
    return this._value;
  }
  
  get number () {
    if (this._value instanceof Decimal) {
      return this._value.toNumber();
    } else {
      return null;
    }
  }

  get readOnly () {
    return this._readOnly;
  }

  set readOnly (value) {
    this._readOnly = value;
  }

  isCompatible (another) {
    if( another instanceof StoreObject) {
      return this.type.isCompatible(another.type);
    }
    return false;
  }
}