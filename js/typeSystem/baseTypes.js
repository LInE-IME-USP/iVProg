class BaseType {
  constructor(name, ord) {
    this.name = name;
    this.ord = ord;
  }

  get value () {
    return this.name;
  }

  isCompatible (another) {
    if(another instanceof BaseType) {
      return this.name === another.name && this.ord === another.ord;
    }
    return false;
  }
}

export const BaseTypes = Object.freeze({
  INTEGER: new BaseType("int", 0),
  REAL: new BaseType("real", 1),
  STRING: new BaseType("string", 2),
  BOOLEAN: new BaseType("bool", 3),
  VOID: new BaseType("void", 4),
  UNDEFINED: new BaseType("undefined", 5)
})
