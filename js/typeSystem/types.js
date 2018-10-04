import { BaseTypes } from './baseTypes';
import { Type } from "./type";
import { MultiType } from "./multiType";

const INTEGER = new Type(BaseTypes.INTEGER);
const REAL = new Type(BaseTypes.REAL);
const STRING = new Type(BaseTypes.STRING);
const BOOLEAN = new Type(BaseTypes.BOOLEAN);
const VOID = new Type(BaseTypes.VOID);
const UNDEFINED = new Type(BaseTypes.UNDEFINED);
const ALL = new MultiType([INTEGER, REAL, STRING, BOOLEAN]);

export const Types = Object.freeze({
  INTEGER: INTEGER,
  REAL: REAL,
  STRING: STRING,
  BOOLEAN: BOOLEAN,
  VOID: VOID,
  UNDEFINED: UNDEFINED,
  ALL: ALL
});
