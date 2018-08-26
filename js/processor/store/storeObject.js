export class StoreObject {

  constructor(type, value, readOnly = false) {
    this.type = type;
    this.value = value;
    this.readOnly = readOnly;
  }

  isCompatible (another) {
    if (another instanceof StoreObject)
     return this.type === another.type;
    return false;
  }
}