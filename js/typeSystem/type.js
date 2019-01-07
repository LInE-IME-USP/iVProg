import { Config } from "../util/config";
import { Types } from "./types";
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
      let result = this.baseType.isCompatible(another.baseType);
      if(result) {
        return true;
      } else if(Config.enable_type_casting) {
        if (this.baseType === BaseTypes.INTEGER || this.baseType === BaseTypes.REAL) {
          if (another.baseType === BaseTypes.INTEGER || another.baseType === BaseTypes.REAL) {
            return true;
          }
        }
      }
    }
    return false;
  }
}
