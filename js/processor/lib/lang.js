import { StoreObject } from '../store/storeObject';
import * as Commands from './../../ast/commands';
import { Types } from './../../typeSystem/types';
import { toReal, convertToString } from "./../../typeSystem/parsers";
import { IVProgParser } from '../../ast/ivprogParser';
import { RealLiteral, IntLiteral, BoolLiteral } from '../../ast/expressions';
import { Modes } from '../modes';
import { MultiType } from '../../typeSystem/multiType';
import { ProcessorErrorFactory } from '../error/processorErrorFactory';

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
    sto.mode = Modes.RETURN;
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
    sto.mode = Modes.RETURN;
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
    sto.mode = Modes.RETURN;
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
    let value = val.value;
    switch (val.type.ord) {
      case Types.INTEGER.ord: {
        value = value.toNumber();
        const temp = new StoreObject(Types.REAL, toReal(value));
        sto.mode = Modes.RETURN;
        return Promise.resolve(sto.updateStore("$", temp));
      }
      case Types.STRING.ord: {
        const parser = IVProgParser.createParser(value);
        try {
          const result = parser.parseTerm();
          if (result instanceof RealLiteral) {
            const temp = new StoreObject(Types.REAL, result.value);
            sto.mode = Modes.RETURN;
            return Promise.resolve(sto.updateStore("$", temp));
          }
        } catch (error) { }
      }
    }
    const typeStringInfoArray = Types.REAL.stringInfo();
    const typeInfo = typeStringInfoArray[0];
    return Promise.reject(ProcessorErrorFactory.invalid_type_conversion(value, typeInfo.type, typeInfo.dim));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(castRealFun)]);
  const func = new Commands.Function('$castReal', Types.REAL,
    [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.STRING]), 'val', false)],
    block);
  return func;
}

export function createCastIntFun () {
  const castIntFun = (sto, _) => {
    const val = sto.applyStore("val");
    let value = val.value;
    switch (val.type.ord) {
      case Types.REAL.ord: {
        value = value.toNumber();
        const temp = new StoreObject(Types.INTEGER, Math.floor(value));
        sto.mode = Modes.RETURN;
        return Promise.resolve(sto.updateStore("$", temp));
      }
      case Types.STRING.ord: {
        const parser = IVProgParser.createParser(value);
        try {
          const result = parser.parseTerm();
          if (result instanceof IntLiteral) {
            const temp = new StoreObject(Types.INTEGER, result.value);
            sto.mode = Modes.RETURN;
            return Promise.resolve(sto.updateStore("$", temp));
          }
        } catch (error) { }
      }
    }
    const typeStringInfoArray = Types.INTEGER.stringInfo();
    const typeInfo = typeStringInfoArray[0];
    return Promise.reject(ProcessorErrorFactory.invalid_type_conversion(value, typeInfo.type, typeInfo.dim));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(castIntFun)]);
  const func = new Commands.Function('$castInt', Types.INTEGER,
    [new Commands.FormalParameter(new MultiType([Types.REAL, Types.STRING]), 'val', false)],
    block);
  return func;
}

export function createCastBoolFun () {
  const castBoolFun = (sto, _) => {
    const str = sto.applyStore("str");
    let value = str.value; 
    const parser = IVProgParser.createParser(value);
    try {
      const val = parser.parseTerm();
      if (val instanceof BoolLiteral) {
        const temp = new StoreObject(Types.BOOLEAN, val.value);
        sto.mode = Modes.RETURN;
        return Promise.resolve(sto.updateStore("$", temp));
      }
    } catch (error) { }
    const typeStringInfoArray = Types.BOOLEAN.stringInfo();
    const typeInfo = typeStringInfoArray[0];
    return Promise.reject(ProcessorErrorFactory.invalid_type_conversion(value, typeInfo.type, typeInfo.dim));
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
    let result = convertToString(val)
    const temp = new StoreObject(Types.STRING, result);
    sto.mode = Modes.RETURN;
    return Promise.resolve(sto.updateStore("$", temp));
  }
  const block = new Commands.CommandBlock([], [new Commands.SysCall(castStringFun)]);
  const func = new Commands.Function('$castString', Types.STRING,
    [new Commands.FormalParameter(Types.ALL, 'str', false)],
    block);
  return func;
}