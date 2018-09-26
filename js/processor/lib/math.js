import { StoreObject } from '../store/storeObject';
import * as Commands from './../../ast/commands';
import { Types, toReal } from './../../ast/types';
import { BigNumber } from 'bignumber.js';

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
    [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
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
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
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
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
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
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
   block);
 return func;
}

export function createPowFun () {
  const powFun = (sto, _) => {
    const x = sto.applyStore('x');
    const y = sto.applyStore('y');
    const result = toReal(Math.pow(x.number, y.number));
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(powFun)]);
 const func = new Commands.Function('$pow', Types.REAL,
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false),
    new Commands.FormalParameter(Types.REAL, 'y', 0, false)],
   block);
 return func;
}

export function createLogFun () {
  const logFun = (sto, _) => {
    const x = sto.applyStore('x');
    if (x.isNegative()) {
      return Promise.reject("the value passed to log function cannot be negative");
    }
    const result = toReal(Math.log10(x.number));
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(logFun)]);
 const func = new Commands.Function('$log', Types.REAL,
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
   block);
 return func;
}

export function createAbsFun () {
  const absFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = x.value.abs();
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(absFun)]);
 const func = new Commands.Function('$abs', Types.REAL,
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
   block);
 return func;
}

export function createNegateFun () {
  const negateFun = (sto, _) => {
    const x = sto.applyStore('x');
    const result = x.value.negated();
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(negateFun)]);
 const func = new Commands.Function('$negate', Types.REAL,
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
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
   [new Commands.FormalParameter(Types.REAL, 'x', 0, false)],
   block);
 return func;
}

export function createMaxFun () {
  const minFun = (sto, _) => {
    const x = sto.applyStore('x');
    const y = sto.applyStore('y');
    const result = BigNumber.max(x.value, y.value);
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(maxFun)]);
 const func = new Commands.Function('$max', Types.ALL,
   [new Commands.FormalParameter(Types.ALL, 'x', 0, false),
   new Commands.FormalParameter(Types.ALL, 'y', 0, false)],
   block);
 return func;
}

export function createMinFun () {
  const minFun = (sto, _) => {
    const x = sto.applyStore('x');
    const y = sto.applyStore('y');
    const result = BigNumber.max(x.value, y.value);
    const temp = new StoreObject(Types.REAL, result);
    return Promise.resolve(sto.updateStore('$', temp));
  };

 const block = new Commands.CommandBlock([],  [new Commands.SysCall(minFun)]);
 const func = new Commands.Function('$min', Types.ALL,
   [new Commands.FormalParameter(Types.ALL, 'x', 0, false),
   new Commands.FormalParameter(Types.ALL, 'y', 0, false)],
   block);
 return func;
}
