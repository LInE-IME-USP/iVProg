import { StoreObject } from '../store/storeObject';
import * as Commands from './../../ast/commands';
import { Types } from './../../typeSystem/types';
import { toInt } from "./../../typeSystem/parsers";


/*
*  substring
*  length
*  uppercase
*  lowercase
*  charAt
**/

export function createSubstringFun () {
  const substringFun = (sto, _) => {
    const str = sto.applyStore("str");
    const start = sto.applyStore("start");
    const end = sto.applyStore("end");
    const result = str.value.substring(start.value, end.value);
    const temp = new StoreObject(Types.STRING, result);
    return Promise.resolve(sto.updateStore("$", temp));
  };

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(substringFun)]);
  const func = new Commands.Function('$substring', Types.STRING,
    [new Commands.FormalParameter(Types.STRING, 'str', false),
    new Commands.FormalParameter(Types.INTEGER, 'start', false),
    new Commands.FormalParameter(Types.INTEGER, 'end', false)],
    block);
  return func;
}

export function createLengthFun () {
  const lengthFun = (sto, _) => {
    const str = sto.applyStore("str");
    const temp = new StoreObject(Types.INTEGER, toInt(value.length));
    return Promise.resolve(sto.updateStore("$", temp));
  }
  const block = new Commands.CommandBlock([],  [new Commands.SysCall(lengthFun)]);
  const func = new Commands.Function('$length', Types.INTEGER,
    [new Commands.FormalParameter(Types.STRING, 'str', false)],
    block);
  return func;
}

export function createUppercaseFun () {
  const uppercaseFun = (sto, _) => {
    const str = sto.applyStore("str");
    const temp = new StoreObject(Types.STRING, str.value.toUpperCase());
    return Promise.resolve(sto.updateStore("$", temp));
  }
  const block = new Commands.CommandBlock([],  [new Commands.SysCall(uppercaseFun)]);
  const func = new Commands.Function('$uppercase', Types.STRING,
    [new Commands.FormalParameter(Types.STRING, 'str', false)],
    block);
  return func;
}

export function createLowercaseFun () {
  const lowercaseFun = (sto, _) => {
    const str = sto.applyStore("str");
    const temp = new StoreObject(Types.STRING, str.value.toLowerCase());
    return Promise.resolve(sto.updateStore("$", temp));
  }
  const block = new Commands.CommandBlock([],  [new Commands.SysCall(lowercaseFun)]);
  const func = new Commands.Function('$lowercase', Types.STRING,
    [new Commands.FormalParameter(Types.STRING, 'str', false)],
    block);
  return func;
}

export function createrCharAtFun () {
  const charAtFun = (sto, _) => {
    const str = sto.applyStore("str");
    const idx = sto.applyStore("index");
    if (idx.value < 0 || idx.value >= str.value.length) {
      return Promise.reject(new Error("invalid string position"));
    }
    const temp = new StoreObject(Types.STRING, str.value.charAt(idx.value));
    return Promise.resolve(sto.updateStore("$", temp));
  }
  const block = new Commands.CommandBlock([],  [new Commands.SysCall(charAtFun)]);
  const func = new Commands.Function('$charAt', Types.STRING,
    [new Commands.FormalParameter(Types.STRING, 'str', false),
    new Commands.FormalParameter(Types.INTEGER, 'index', false)],
    block);
  return func;
}
