import { StoreObject } from './storeObject';

export class StoreObjectRef extends StoreObject {

  constructor (id, store) {
    super(null, null, false);
    this.setID(id);
    this.store = store;
  }

  get isRef () {
    return true;
  }

  get type () {
    return this.store.applyStore(this.id).type;
  }

  get value () {
    return this.store.applyStore(this.id).value;
  }

  getRefObj () {
    return this.store.applyStore(this.id);
  }

  updateRef (stoObj) {
    this.store.updateStore(this.id, stoObj);
  }

  isCompatible (another) {
    return this.store.applyStore(this.id).isCompatible(another);
  }
}