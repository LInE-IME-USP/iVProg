export class Type {

  constructor(baseType) {
    this.baseType = baseType;
  }

  get value () {
    return this.baseType.value;
  }

  get ord () {
    return this.baseType.ord;
  }

  isCompatible (another) {
    if(another instanceof Type) {
      return this.baseType.isCompatible(another.baseType);
    }
    return false;
  }
}
