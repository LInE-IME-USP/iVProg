import * as Commands from './../ast/commands';
import {Types} from './../ast/types';

function createOutputFun () {
  const block = new Commands.CommandBlock([], [new Commands.SysCall('$write')]);
  const func = new Commands.Function('$write', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', 0, false)],
    block);
  return func;
}

function createInputFun () {
  const block = new Commands.CommandBlock([],  [new Commands.SysCall('$read')]);
  const func = new Commands.Function('$read', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', 0, true)],
    block);
  return func;
}

export const LanguageDefinedFunction = Object.freeze({
  $write: createOutputFun(),
  $read: createInputFun()
});

export const NAMES = Object.freeze({
  WRITE: '$write',
  READ: '$read'
});