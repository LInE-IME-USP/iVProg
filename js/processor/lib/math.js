import { StoreObject } from '../store/storeObject';
import * as Commands from './../../ast/commands';
import { Types } from './../../typeSystem/types';
import { toReal } from "./../../typeSystem/parsers";
import { BigNumber } from 'bignumber.js';
import { MultiType } from '../../typeSystem/multiType';
import { CompoundType } from '../../typeSystem/compoundType';

/**
 * sin
 * cos
 * tan
 * sqrt
 * pow
 * log
 * abs
 * negate
 * invert
 * max
 * min
 */

export function createSinFun () {
   const sinFun = (sto, _) => {
     const x = sto.applyStore('x');
     const result = toReal(Math.sin(x.number));
     const temp = new StoreObject(Types.REAL, result);
     return Promise.resolve(sto.updateStore('$', temp));
   };

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(sinFun)]);
  const func = new Commands.Function('$sin', Types.REAL,
    [new Commands.FormalParameter(new MultiType([[Types.INTEGER, Types.REAL]]), 'x', false)],
    block);
  return func;
}

export function createCosFun () {
  const cosFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = toReal(Math.cos(x.number));
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(cosFun)]);
 const func = new Commands.Function('$cos', Types.REAL,
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false)],
   block);
 return func;
}

export function createTanFun () {
  const tanFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = toReal(Math.tan(x.number));
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(tanFun)]);
 const func = new Commands.Function('$tan', Types.REAL,
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false)],
   block);
 return func;
}

export function createSqrtFun () {
  const sqrtFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = x.value.sqrt();
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(sqrtFun)]);
 const func = new Commands.Function('$sqrt', Types.REAL,
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false)],
   block);
 return func;
}

export function createPowFun () {
  const powFun = (sto, _) => {
    const x = sto.applyStore('x');f
    const y = sto.applyStore('y');
    const result = toReal(Math.pow(x.number, y.number));
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(powFun)]);
 const func = new Commands.Function('$pow', Types.REAL,
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false),
    new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'y', false)],
   block);
 return func;
}

export function createLogFun () {
  const logFun = (sto, _) => {
    const x = sto.applyStore('x');
    if (x.value.isNegative()) {
      return Promise.reject("the value passed to log function cannot be negative");
    }
    const result = toReal(Math.log10(x.number));
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(logFun)]);
 const func = new Commands.Function('$log', Types.REAL,
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false)],
   block);
 return func;
}

export function createAbsFun () {
  const absFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = x.value.abs();
    const temp = new StoreObject(x.type, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(absFun)]);
 const func = new Commands.Function('$abs', new MultiType([Types.INTEGER, Types.REAL]),
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false)],
   block);
 return func;
}

export function createNegateFun () {
  const negateFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = x.value.negated();
    const temp = new StoreObject(x.type, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(negateFun)]);
 const func = new Commands.Function('$negate', new MultiType([Types.INTEGER, Types.REAL]),
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false)],
   block);
 return func;
}

export function createInvertFun () {
  const invertFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = toReal(1).dividedBy(x.value);
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(invertFun)]);
 const func = new Commands.Function('$invert', Types.REAL,
   [new Commands.FormalParameter(new MultiType([Types.INTEGER, Types.REAL]), 'x', false)],
   block);
 return func;
}

export function createMaxFun () {
  const maxFun = (sto, _) => {
    const x = sto.applyStore('x');
    const numbers = x.value.map(stoObj => stoObj.number);
    const result = BigNumber.max(numbers);
    const temp = new StoreObject(x.type.innerType, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };
 const paramType = new CompoundType(new MultiType([Types.INTEGER, Types.REAL]), 1);
 const block = new Commands.CommandBlock([],  [new Commands.SysCall(maxFun)]);
 const func = new Commands.Function('$max', new MultiType([Types.INTEGER, Types.REAL]),
   [new Commands.FormalParameter(paramType, 'x', false)],
   block);
 return func;
}

export function createMinFun () {
  const minFun = (sto, _) => {
    const x = sto.applyStore('x');
    const numbers = x.value.map(stoObj => stoObj.value.toNumber());
    const result = BigNumber.min(numbers);
    const temp = new StoreObject(x.type.innerType, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };
 const paramType = new CompoundType(new MultiType([Types.INTEGER, Types.REAL]), 1);
 const block = new Commands.CommandBlock([],  [new Commands.SysCall(minFun)]);
 const func = new Commands.Function('$min', new MultiType([Types.INTEGER, Types.REAL]),
   [new Commands.FormalParameter(paramType, 'x', false)],
   block);
 return func;
}
