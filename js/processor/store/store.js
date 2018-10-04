import { Modes } from './../modes';

export class Store {

  constructor() {
    this.store = {};
    this.nextStore = null;
    this.mode = Modes.RUN; 
  }

  extendStore (nextStore) {
    this.nextStore = nextStore;
  }

  applyStore (id) {
    if(!this.store[id]) {
      if (this.nextStore !== null) {
        return this.nextStore.applyStore(id);
      } else {
        // TODO: better error message
        throw new Error(`Variable ${id} not found.`);
      }
    }
    const val = this.store[id];
    if (val.isRef) {
      return val.getRefObj();
    }
    return this.store[id];
  }

  updateStore (id, stoObj) {
    if(!this.store[id]) {
      if(this.nextStore !== null) {
        this.nextStore.updateStore(id, stoObj);
        return this;
      } else {
        // TODO: better error message
        throw new Error(`Variable ${id} not found.`);
      }
    } else {
      const oldObj = this.store[id];
      if(oldObj.readOnly) {
        // TODO: better error message
        throw new Error("Cannot change value of a read only variable: " + id);
      }
      if(oldObj.isRef) {
        oldObj.updateRef(stoObj);
        return this;
      } else if(oldObj.isCompatible(stoObj)) {
        stoObj.setID(id);
        this.store[id] = Object.freeze(stoObj);
        return this;
      } else {
        const oldType = oldObj.type;
        const stoType = stoObj.type;
        // TODO: better error message
        console.log(oldObj);
        console.log(stoObj);
        throw new Error(`${oldType} is not compatible with type ${stoType} given`);
      }
    }
  }

  //In case of future use of ref, it needs to have a special function to update the storeRefObject
  // and no the StoreObject refferenced by it
  // updateStoreRef(id, stoObjAddress) {...}

  insertStore (id, stoObj) {
    if (this.store[id]) {
      // TODO: better error message
      throw new Error(`${id} is already defined`);
    }
    stoObj.setID(id);
    this.store[id] = Object.freeze(stoObj);
    return this;
  }
}