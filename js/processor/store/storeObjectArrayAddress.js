import { StoreObject } from './storeObject';
import { StoreObjectArray } from './storeObjectArray';
import { CompoundType } from '../../typeSystem/compoundType';

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
      const refColumn = refLine.value[this.column];
      return refColumn;
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
    if(!(this.type instanceof CompoundType)) {
      return null;
    }
    return this.refValue.value.length;
  }

  get columns () {
    switch (this.type.dimensions) {
      case 2:
        return this.refValue.value[0].value.length;
      default:
        return null;
    }
  }

  getArrayObject () {
    return this.store.applyStore(this.refID);
  }

  updateArrayObject (stoObj) {
    const anArray =  this.getArrayObject();
    const newArray = Object.assign(new StoreObjectArray(null,null,null), anArray);
    if(!stoObj.type.isCompatible(this.type)) {
      throw new Error(`Invalid operation: cannot assign the value given to ${this.refID}`);
    } else if (this.type instanceof CompoundType && this.type.canAccept(stoObj.type)) {
      throw new Error(`Invalid operation: cannot assign the value given to ${this.refID}`);
    }
    if (this.column !== null) {
     newArray.value[this.line].value[this.column] = stoObj;
     return newArray;
    } else {
     newArray.value[this.line] = stoObj;
     return newArray;
    }
  }

  isCompatible (another) {
    if(this.type.isCompatible(another.type)) {
      if(another.type instanceof CompoundType) {
        return this.lines === another.lines && this.columns === another.columns;
      } else {
        this.refValue.isCompatible(another);
      }
    }
  }
}
