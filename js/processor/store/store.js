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
    return this.store[id];
  }

  updateStore (id, stoObj) {
    if(!this.store[id]) {
      if(this.extendStore !== null) {
        return this.extendStore.updateStore(id, stoObj);
      } else {
        // TODO: better error message
        throw new Error(`Variable ${id} not found.`);
      }
    } else {
      const oldObj = this.applyStore(id);
      if(oldObj.readOnly) {
        // TODO: better error message
        throw new Error("Cannot change value of a read only variable: " + id);
      }
      if(oldObj.isCompatible(stoObj)) {
        this.store[id] = Object.freeze(stoObj);
        return this;
      } else {
        // TODO: better error message
        throw new Error(`${stoObj.type} is not compatible with the value given`);
      }
    }
  }

  insertStore (id, stoObj) {
    if (this.store[id]) {
      // TODO: better error message
      throw new Error(`${id} is already defined`);
    }
    this.store[id] = stoObj;
    return this;
  }
}