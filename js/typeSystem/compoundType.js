import { Type } from "./type";

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

  canAccept (another) {
    if(another instanceof CompoundType) {
      return this.dimensions > another.dimensions && this.innerType.isCompatible(another.innerType);
    } else {
      return this.innerType.isCompatible(another);
    }
  }
}
