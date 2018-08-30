import { StoreObject } from './storeObject';

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