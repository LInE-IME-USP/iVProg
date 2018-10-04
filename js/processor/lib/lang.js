import { StoreObject } from '../store/storeObject';
import * as Commands from './../../ast/commands';
import { Types } from './../../typeSystem/types';
import { toReal, convertBoolToString } from "./../../typeSystem/parsers";
import { IVProgParser } from '../../ast/ivprogParser';
import { RealLiteral, IntLiteral, BoolLiteral } from '../../ast/expressions';

/**
 * 
 * is_real
 * is_int
 * is_bool
 * cast_real
 * cast_int
 * cast_bool
 * cast_string
 */

export function createIsRealFun () {
  const isRealFun = (sto, _) => {
    const str = sto.applyStore("str");
    const parser = IVProgParser.createParser(str.value);
    let result = false;
    try {
      const val = parser.parseTerm();
      if (val instanceof RealLiteral) {
        result = true;
      }
    } catch (error) { }
    const temp = new StoreObject(Types.BOOLEAN, result);
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(isRealFun)]);
  const func = new Commands.Function('$isReal', Types.BOOLEAN,
    [new Commands.FormalParameter(Types.STRING, 'str', false)],
    block);
  return func;
}

export function createIsIntFun () {
  const isIntFun = (sto, _) => {
    const str = sto.applyStore("str");
    const parser = IVProgParser.createParser(str.value);
    let result = false;
    try {
      const val = parser.parseTerm();
      if (val instanceof IntLiteral) {
        result = true;
      }
    } catch (error) { }
    const temp = new StoreObject(Types.BOOLEAN, result);
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(isIntFun)]);
  const func = new Commands.Function('$isInt', Types.BOOLEAN,
    [new Commands.FormalParameter(Types.STRING, 'str', false)],
    block);
  return func;
}

export function createIsBoolFun () {
  const isBoolFun = (sto, _) => {
    const str = sto.applyStore("str");
    const parser = IVProgParser.createParser(str.value);
    let result = false;
    try {
      const val = parser.parseTerm();
      if (val instanceof BoolLiteral) {
        result = true;
      }
    } catch (error) { }
    const temp = new StoreObject(Types.BOOLEAN, result);
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(isBoolFun)]);
  const func = new Commands.Function('$isBool', Types.BOOLEAN,
    [new Commands.FormalParameter(Types.STRING, 'str', false)],
    block);
  return func;
}

export function createCastRealFun () {
  const castRealFun = (sto, _) => {
    const val = sto.applyStore("val");
    switch (val.type.ord) {
      case Types.INTEGER.ord: {
        const temp = new StoreObject(Types.REAL, toReal(val.number));
        return Promise.resolve(sto.updateStore("$", temp));
      }
      case Types.STRING.ord: {
        const parser = IVProgParser.createParser(val.value);
        try {
          const result = parser.parseTerm();
          if (result instanceof RealLiteral) {
            const temp = new StoreObject(Types.REAL, result.value);
            return Promise.resolve(sto.updateStore("$", temp));
          }
        } catch (error) { 
          return Promise.reject("cannot convert string to real");
        }
      }
    }
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(castRealFun)]);
  const func = new Commands.Function('$castReal', Types.REAL,
    [new Commands.FormalParameter(Types.ALL, 'val', false)],
    block);
  return func;
}

export function createCastIntFun () {
  const castIntFun = (sto, _) => {
    const val = sto.applyStore("val");
    switch (val.type.ord) {
      case Types.REAL.ord: {
        const temp = new StoreObject(Types.INTEGER, Math.floor(val.number));
        return Promise.resolve(sto.updateStore("$", temp));
      }
      case Types.STRING.ord: {
        const parser = IVProgParser.createParser(val.value);
        try {
          const result = parser.parseTerm();
          if (result instanceof IntLiteral) {
            const temp = new StoreObject(Types.INTEGER, result.value);
            return Promise.resolve(sto.updateStore("$", temp));
          }
        } catch (error) { 
          return Promise.reject("cannot convert string to real");
        }
      }
    }
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(castIntFun)]);
  const func = new Commands.Function('$castInt', Types.INTEGER,
    [new Commands.FormalParameter(Types.ALL, 'val', false)],
    block);
  return func;
}

export function createCastBoolFun () {
  const castBoolFun = (sto, _) => {
    const str = sto.applyStore("str");
    const parser = IVProgParser.createParser(str.value);
    try {
      const val = parser.parseTerm();
      if (val instanceof BoolLiteral) {
        const temp = new StoreObject(Types.BOOLEAN, val.value);
        return Promise.resolve(sto.updateStore("$", temp));
      }
    } catch (error) { }
    return Promise.reject("cannot convert " + str.value + " to boolean");
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(castBoolFun)]);
  const func = new Commands.Function('$castBool', Types.BOOLEAN,
    [new Commands.FormalParameter(Types.STRING, 'str', false)],
    block);
  return func;
}

export function createCastStringFun () {
  const castStringFun = function (store, _) {
    const val = store.applyStore('str');
    if(val.type.isCompatible(Types.INTEGER)) {
      this.output.sendOutput(val.value.toString());
    } else if (val.type.isCompatible(Types.REAL)) {
      if (val.value.dp() <= 0) {
        this.output.sendOutput(val.value.toFixed(1));  
      } else {
        this.output.sendOutput(val.value.toString());
      }
    } else {
      this.output.sendOutput(val.value);
    }
    let result = null;
    switch (val.type.ord) {
      case Types.INTEGER.ord:
        result = val.value.toString();  
        break;
      case Types.REAL.ord: {
        if (val.value.dp() <= 0) {
          result = val.value.toFixed(1);  
        } else {
          result = val.number;
        }
        break;
      }
      case Types.BOOLEAN.ord:
        result = convertBoolToString(val.value);
        break;
      default:
        result = val.value;
        break;
    }
    const temp = new StoreObject(Types.STRING, result);
    return Promise.resolve(sto.updateStore("$", temp));
  }
  const block = new Commands.CommandBlock([], [new Commands.SysCall(castStringFun)]);
  const func = new Commands.Function('$castString', Types.STRING,
    [new Commands.FormalParameter(Types.ALL, 'str', false)],
    block);
  return func;
}