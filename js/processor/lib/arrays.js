import { StoreObject } from '../store/storeObject';
import * as Commands from './../../ast/commands';
import { Types } from './../../typeSystem/types';
import { toInt } from "./../../typeSystem/parsers";
import { CompoundType } from '../../typeSystem/compoundType';
import { Modes } from '../modes';

/**
 * num_elements
 * matrix_lines
 * matrix_columns
 */

export function createNumElementsFun () {
  const numElementsFun = (sto, _) => {
    const vector  = sto.applyStore("vector");
    const temp = new StoreObject(Types.INTEGER, toInt(vector.lines));
    sto.mode = Modes.RETURN;
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(numElementsFun)]);
  const func = new Commands.Function('$numElements', Types.INTEGER,
    [new Commands.FormalParameter(new CompoundType(Types.ALL, 1), 'vector', false)],
    block);
  return func;
 }

export function createMatrixLinesFun () {
  const matrixLinesFun = (sto, _) => {
    const matrix  = sto.applyStore("matrix");
    const temp = new StoreObject(Types.INTEGER, toInt(matrix.lines));
    sto.mode = Modes.RETURN;
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(matrixLinesFun)]);
  const func = new Commands.Function('$matrixLines', Types.INTEGER,
    [new Commands.FormalParameter(new CompoundType(Types.ALL, 2), 'matrix', false)],
    block);
  return func;
 }

export function createMatrixColumnsFun () {
  const matrixColumnsFun = (sto, _) => {
    const matrix  = sto.applyStore("matrix");
    const temp = new StoreObject(Types.INTEGER, toInt(matrix.columns));
    sto.mode = Modes.RETURN;
    return Promise.resolve(sto.updateStore("$", temp));
  }

  const block = new Commands.CommandBlock([],  [new Commands.SysCall(matrixColumnsFun)]);
  const func = new Commands.Function('$matrixColumns', Types.INTEGER,
    [new Commands.FormalParameter(new CompoundType(Types.ALL, 2), 'matrix', false)],
    block);
  return func;
 }
 