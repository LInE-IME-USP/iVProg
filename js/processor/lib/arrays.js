import { StoreObject } from '../store/storeObject';
import * as Commands from './../../ast/commands';
import { Types } from './../../ast/types';

/**
 * num_elements
 * matrix_lines
 * matrix_columns
 */

export function createNumElementsFun () {
  const numElementsFun = (sto, _) => {
    const vector  = sto.applyStore("vector");
    const temp = new StoreObject(Types.INTEGER, vector.lines);
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(numElementsFun)]);
  const func = new Commands.Function('$numElements', Types.INTEGER,
    [new Commands.FormalParameter(Types.ALL, 'vector', 1, false)],
    block);
  return func;
 }

export function createMatrixLinesFun () {
  const matrixLinesFun = (sto, _) => {
    const matrix  = sto.applyStore("matrix");
    const temp = new StoreObject(Types.INTEGER, matrix.lines);
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(matrixLinesFun)]);
  const func = new Commands.Function('$matrixLines', Types.INTEGER,
    [new Commands.FormalParameter(Types.ALL, 'matrix', 2, false)],
    block);
  return func;
 }

export function createMatrixColumnsFun () {
  const matrixColumnsFun = (sto, _) => {
    const matrix  = sto.applyStore("matrix");
    const temp = new StoreObject(Types.INTEGER, matrix.columns);
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(matrixColumnsFun)]);
  const func = new Commands.Function('$matrixColumns', Types.INTEGER,
    [new Commands.FormalParameter(Types.ALL, 'matrix', 2, false)],
    block);
  return func;
 }
 