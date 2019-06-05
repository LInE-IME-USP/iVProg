import { StoreObject } from './../store/storeObject';
import * as Commands from './../../ast/commands';
import { Modes } from '../modes';
import {toInt, toString, toBool, toReal, convertToString} from './../../typeSystem/parsers';
import { Types } from './../../typeSystem/types';
import { ProcessorErrorFactory } from "./../error/processorErrorFactory";
import { StoreObjectArrayAddressRef } from '../store/storeObjectArrayAddressRef';

export function createOutputFun () {
  const writeFunction = function (store, _) {
    const val = store.applyStore('p1');
    this.output.sendOutput(convertToString(val.value, val.type));
    store.mode = Modes.RETURN;
    return Promise.resolve(store);
  }
  const block = new Commands.CommandBlock([], [new Commands.SysCall(writeFunction)]);
  const func = new Commands.Function('$write', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', false)],
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
      try {
        if (typeToConvert.isCompatible(Types.INTEGER)) {
          const val = toInt(text);
          stoObj = new StoreObject(Types.INTEGER, val.trunc());
        } else if (typeToConvert.isCompatible(Types.REAL)) {
          stoObj = new StoreObject(Types.REAL, toReal(text));
        } else if (typeToConvert.isCompatible(Types.BOOLEAN)) {
          stoObj = new StoreObject(Types.BOOLEAN, toBool(text));
        } else if (typeToConvert.isCompatible(Types.STRING)) {
          stoObj = new StoreObject(Types.STRING, toString(text));
        } else {
          return Promise.reject(new Error("!!!!Critical error: Unknown type in readFunction!!!!"));
        }  
      } catch (_) {
        const stringInfo = typeToConvert.stringInfo()[0]
        const realObject = store.getStoreObject("p1");
        if (realObject instanceof StoreObjectArrayAddressRef) {
          const arrayInfo = realObject.address.getArrayObject().type.stringInfo()[0];
          const error = ProcessorErrorFactory.invalid_read_type_array(text, stringInfo.type, stringInfo.dim, realObject.address.refID, arrayInfo.type, arrayInfo.dim);
          return Promise.reject(error);
        }
        const error = ProcessorErrorFactory.invalid_read_type(text, stringInfo.type, stringInfo.dim, store.applyStore('p1').id);
        return Promise.reject(error);
      }
      this.loopTimers.splice(0,this.loopTimers.length)
      store.updateStore('p1', stoObj);
      store.mode = Modes.RETURN;
      return Promise.resolve(store);
    });
  }
  const block = new Commands.CommandBlock([],  [new Commands.SysCall(readFunction)]);
  const func = new Commands.Function('$read', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', true)],
    block);
  return func;
}