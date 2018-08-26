import { StoreObject } from './storeObject';
import { Types } from './../../ast/types';

export class StoreObjectArray extends StoreObject {

  constructor (subtype, lines, columns, value, readonly) {
    super(Types.ARRAY, value, readonly);
    this.subtype = subtype;
    this.lines = lines;
    this.columns = columns;
  }

  isCompatible (another) {
    if (another instanceof StoreObjectArray) {
      if (this.subtype === another.subtype &&
        this.lines === another.lines &&
        this.columns === another.columns) {
        return super.isCompatible(another);
      }
    }
    return false;
  }
}