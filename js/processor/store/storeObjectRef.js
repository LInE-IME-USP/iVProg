import { StoreObject } from './storeObject';
import Decimal from 'decimal.js';

export class StoreObjectRef extends StoreObject {

  constructor (refID, store) {
    super(null, null, false);
    this.refID = refID;
    this.store = store;
  }

  get isRef () {
    return true;
  }

  get type () {
    return this.store.applyStore(this.refID).type;
  }

  get value () {
    return this.store.applyStore(this.refID).value;
  }

  get number () {
    if (this.value instanceof Decimal) {
      return this.value.toNumber();
    } else {
      return null;
    }
  }

  getRefObj () {
    return this.store.applyStore(this.refID);
  }

  updateRef (stoObj) {
    this.store.updateStore(this.refID, stoObj);
  }

  isCompatible (another) {
    return this.store.applyStore(this.refID).isCompatible(another);
  }
}