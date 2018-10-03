import { Type } from "./types";

export class CompoundType extends Type {

  constructor (type, dimensions) {
    super(null);
    this.innerType = type;
    this.dimensions = dimensions;
  }

  isCompatible (another) {
    if(another instanceof CompoundType){
      if(this.dimensions !== another.dimensions) {
        return false;
      }
      return this.innerType.isCompatible(another.innerType);
    }
    return false;
  }
}
