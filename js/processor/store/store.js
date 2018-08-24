export class Store {

  constructor () {
    this.store = {};
    this.nextStore = null;
  }

  extendStore (nextStore) {
    return Object.assign(new Store, {
      store: this.store,
      nextStore: nextStore
    });
  }

  findVariable (id) {
    if(!this.store[id]) {
      if (this.nextStore === null) {
        throw new Error("Undefined variable: " + id);
      } else {
        return this.nextStore.findVariable(id);
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

  updateVariable (id, storeObj) {
    if(!this.isDefined(id)) {
      this.store[id] =  storeObj;
    } else {
      const old = this.findVariable(id);
      if (storeObj.type !== old.type) {
        throw new Error(`${storeObj.value} is not compatible with ${old.type}`);
      } else {
        this.store[id] = storeObj;
      }
    }
  }
}