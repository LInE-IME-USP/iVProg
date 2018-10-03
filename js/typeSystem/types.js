import { BaseTypes } from './baseTypes';
import { MultiType } from "./multiType";

export class Type {

  constructor(baseType) {
    this.baseType = baseType;
  }

  get value () {
    return this.baseType.value;
  }

  get ord () {
    return this.baseType.ord;
  }

  isCompatible (another) {
    if(another instanceof Type) {
      return this.baseType.isCompatible(another.baseType);
    }
    return false;
  }
}


const INTEGER = new Type(BaseTypes.INTEGER);
const REAL = new Type(BaseTypes.REAL);
const STRING = new Type(BaseTypes.STRING);
const BOOLEAN = new Type(BaseTypes.BOOLEAN);
const VOID = new Type(BaseTypes.VOID);
const UNDEFINED = new Type(BaseTypes.UNDEFINED);
const ALL = new MultiType(innerTypes.INTEGER, innerTypes.REAL, innerTypes.STRING, innerTypes.BOOLEAN);

export const Types = Object.freeze({
  INTEGER: INTEGER,
  REAL: REAL,
  STRING: STRING,
  BOOLEAN: BOOLEAN,
  VOID: VOID,
  UNDEFINED: UNDEFINED,
  ALL: ALL
});
