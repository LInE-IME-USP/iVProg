import { Types } from './../types';

export class Function {

  constructor(name, returnType, formalParameters, commandBlock) {
    this.name = name;
    this.returnType = returnType;
    this.formalParameters = formalParameters;
    this.commandBlock = commandBlock;
  }

  get isMain () {
    return this.name === null && this.returnType === Types.VOID;
  }

  get commands () {
    return this.commandBlock.commands;
  }

  get variablesDeclarations () {
    return this.commandBlock.variables;
  }
}