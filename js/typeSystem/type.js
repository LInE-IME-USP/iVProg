import { Config } from "../util/config";
import { BaseTypes } from "./baseTypes";

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

  stringInfo () {
    return [{type: this.baseType.name, dim: 0}];
  }

  isCompatible (another) {
    if(another instanceof Type) {
      return this.baseType.isCompatible(another.baseType);
    }
    return false;
  }
}
