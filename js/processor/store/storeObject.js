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

  get readOnly () {
    return this._readOnly;
  }

  isCompatible (another) {
    if( another instanceof StoreObject) {
      return this.type === another.type;
    }
    return false;
  }
}