import { Modes } from './../mode';
export class Store {

  constructor () {
    this.store = {};
    this.nextStore = null;
    this.mode = Modes.RUN;
  }

  cloneStore () {
    return Object.assign(new Store(), this);
  }

  extendStore (nextStore) {
   this.nextStore = nextStore;
  }

  applyStore (id) {
    if(!this.store[id]) {
      if (this.nextStore === null) {
        // TODO: better error message
        throw new Error(`Variable ${id} is not defined.`);
      } else {
        return this.nextStore.applyStore(id);
      }
    }
    return this.store[id];
  }

  isDefined (id) {
    if(!this.store[id]) {
      if (this.nextStore === null) {
        return false;
      } else {
        return this.nextStore.isDefined(id);
      }
    }
    return true;
  }

  updateStore (id, storeObj) {
    if(!this.store[id]) {
      if (this.nextStore !== null) {
        this.nextStore.updateStore(id, storeObj);
      } else {
        throw new Error(`Variable ${id} is not defined.`);  
      }
    } else {
      const old = this.applyStore(id);
      if (!old.isCompatible(storeObj)) {
        // TODO: better error message
        throw new Error(`${storeObj.value} is not compatible with ${id} of type ${old.type}`);
      } else if (old.readOnly) {
        // TODO: better error message
        throw new Error(`Cannot change value, ${id} is read-only`);
      } else {
        this.store[id] = Object.freeze(storeObj);
      }
    }
  }

  insertStore (id, storeObj) {
    if(this.store(id)) {
      // TODO: Better error message
      throw new Error(`Variable ${id} is already defined.`);
    } else {
      this.store[id] = Object.freeze(storeObj)
    }
  }
}