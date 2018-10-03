import { Type } from "./types";

export class MultiType extends Type {

  constructor (...types) {
    super(null);
    this.types = types;
  }

  get value () {
    return null;
  }

  get ord () {
    return null;
  }

  isCompatible (another) {
    if(another instanceof Type) {
      for (let i = 0; i < this.types.length; i++) {
        const t = this.types[i];
        if (t.isCompatible(another)) {
          return true;
        }
      }
      return false;
    }
    return false;
  }
}
