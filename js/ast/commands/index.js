import { Break } from './break';
import { Return } from './return';
import { Assign } from './assign';
import { ArrayIndexAssign } from './arrayAssign';
import { Declaration } from './declaration';
import { ArrayDeclaration } from './arrayDeclaration';
import { While } from './while';
import { For } from './for';
import { Function } from './function';
import { IfThenElse } from './ifThenElse';
import { CommandBlock } from './commandBlock';
import { DoWhile } from './doWhile';
import { Switch } from './switch';
import { Case } from './case';
import { SysCall } from './sysCall';
import { FormalParameter } from './formalParameter';
import { FunctionCall } from './../expressions/functionCall'; //Proxy to expression since they do exatcly the same thing

export {
  Break,
  Return,
  Assign,
  ArrayIndexAssign,
  Declaration,
  ArrayDeclaration,
  While,
  For,
  Function,
  IfThenElse,
  CommandBlock,
  DoWhile,
  Switch,
  Case,
  SysCall,
  FormalParameter,
  FunctionCall
};