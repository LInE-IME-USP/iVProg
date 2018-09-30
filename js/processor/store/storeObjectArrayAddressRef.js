import { StoreObject } from './storeObject';

export class StoreObjectArrayAddressRef extends StoreObject {

  constructor (address) {
    super(null, null, false);
    this.address = address;
  }

  get isRef () {
    return true;
  }

  get type () {
    return this.address.type;
  }

  get value () {
    return this.address.value;
  }

  get number () {
    if (this.value instanceof BigNumber) {
      return this.value.toNumber();
    } else {
      return null;
    }
  }

  getRefObj () {
    return this.address.refValue;
  }

  updateRef (stoObj) {
    const newArray = this.address.updateArrayObject(stoObj);
    this.address.store.updateStore(this.address.refID, newArray);
  }

  isCompatible (another) {
    return this.address.isCompatible(another);
  }
}