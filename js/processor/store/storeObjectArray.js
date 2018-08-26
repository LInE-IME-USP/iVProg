import { Types } from './../../ast/types';
import { StoreObject } from './storeObject';
export class StoreObjectArray extends StoreObject {

  constructor (subtype, lines, columns, value, readOnly) {
    super(Types.ARRAY, value, readOnly);
    this.lines = lines;
    this.columns = columns;
    this.subtype = subtype;
  }

  isCompatible (another) {
    if(another instanceof StoreObjectArray) {
      if(this.lines === another.lines &&
        this.columns === another.columns &&
        this.subtype === another.subtype) {
          return super.isCompatible(another);
        }
    }
    return false;
  }
}