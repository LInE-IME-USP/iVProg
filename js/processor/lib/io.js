import { StoreObject } from './../store/storeObject';
import * as Commands from './../../ast/commands';
import {Types, toInt, toString, toBool, toReal} from './../../ast/types';

export function createOutputFun () {
  const writeFunction = function (store, _) {
    const val = store.applyStore('p1');
    if(val.type === Types.INTEGER) {
      this.output.sendOutput(val.value.toString());
    } else if (val.type === Types.REAL) {
      if (val.value.dp() <= 0) {
        this.output.sendOutput(val.value.toFixed(1));  
      } else {
        this.output.sendOutput(val.value.toString());
      }
    } else {
      this.output.sendOutput(val.value);
    }
    return Promise.resolve(store);
  }
  const block = new Commands.CommandBlock([], [new Commands.SysCall(writeFunction)]);
  const func = new Commands.Function('$write', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', 0, false)],
    block);
  return func;
}

export function createInputFun () {
  const readFunction = function (store, _) {
    const request = new Promise((resolve, _) => {
      this.input.requestInput(resolve);
    });
    return request.then(text => {
      const typeToConvert = store.applyStore('p1').type;
      let stoObj = null;
      if (typeToConvert === Types.INTEGER) {
        const val = toInt(text);
        stoObj = new StoreObject(Types.INTEGER, val);
      } else if (typeToConvert === Types.REAL) {
        stoObj = new StoreObject(Types.REAL, toReal(text));
      } else if (typeToConvert === Types.BOOLEAN) {
        stoObj = new StoreObject(Types.BOOLEAN, toBool(text));
      } else if (typeToConvert === Types.STRING) {
        stoObj = new StoreObject(Types.STRING, toString(text));
      }
      store.updateStore('p1', stoObj);
      return Promise.resolve(store);
    });
  }
  const block = new Commands.CommandBlock([],  [new Commands.SysCall(readFunction)]);
  const func = new Commands.Function('$read', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', 0, true)],
    block);
  return func;
}