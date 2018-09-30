import { StoreObject } from './storeObject';
import { StoreObjectArray } from './storeObjectArray';
import { Types } from '../../ast/types';

export class StoreObjectArrayAddress extends StoreObject {

  constructor (refID, line, column, store) {
    super(null, null, false);
    this.refID = refID;
    this.store = store;
    this.line = line;
    this.column = column;
  }

  get isRef () {
    return false;
  }

  get inStore () {
    return true;
  }

  get refValue () {
    const refLine = this.store.applyStore(this.refID).value[this.line];
    if (this.column !== null) {
      return refLine.value[this.column];
    }
    return refLine;
  }

  get value () {
    return this.refValue.value;
  }

  get type () {
    return this.refValue.type;
  }

  get lines () {
    if(this.type !== Types.ARRAY) {
      return null;
    }
    return this.refValue.value.length;
  }

  get columns () {
    if(this.lines <= 0) {
      return null;
    } else if (this.refValue.value[0].type !== Types.ARRAY) {
      return null;
    }
    // this.refValue is StoreObject
    //this.refValue.value[0] is another StoreObject
    return this.refValue.value[0].value.length;
  }

  get subtype () {
    return this.refValue.subtype;
  }

  getArrayObject () {
    return this.store.applyStore(this.refID);
  }

  updateArrayObject (stoObj) {
    const anArray =  this.getArrayObject();
    const newArray = Object.assign(new StoreObjectArray(null,null,null), anArray);
    if (this.column !== null) {
     if (stoObj.type === Types.ARRAY) {
       throw new Error(`Invalid operation: cannot assign the value given to ${this.refID}`);
     }
     newArray.value[this.line].value[this.column] = stoObj;
     return newArray;
    } else {
     if(anArray.columns !== null && stoObj.type !== Types.ARRAY) {
      throw new Error(`Invalid operation: cannot assign the value given to ${this.refID}`);
     }
     newArray.value[this.line] = stoObj;
     return newArray;
    }
  }

  isCompatible (another) {
    if(this.type === Types.ARRAY) {
      if(another.type !== Types.ARRAY) {
        return false;
      }
      if(another.subtype !== this.subtype) {
        return false;
      }
      return this.lines === another.lines && this.columns === another.columns;
    }
    return this.refValue.isCompatible(another);
  }
}