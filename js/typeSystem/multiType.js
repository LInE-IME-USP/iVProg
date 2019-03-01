import { Type } from "./type";

export class MultiType extends Type {

  constructor (types) {
    super(null);
    this.types = types;
  }

  get value () {
    return null;
  }

  get ord () {
    return null;
  }

  stringInfo () {
    let list = [];
    for (let i = 0; i < this.types.length; i++) {
      const t = this.types[i];
      list = list.concat(t.stringInfo());
    }
    return list;
  }

  isCompatible (another) {
    if(another instanceof Type) {
      for (let i = 0; i < this.types.length; i++) {
        const t = this.types[i];
        if (another.isCompatible(t)) {
          return true;
        }
      }
    }
    return false;
  }
}
