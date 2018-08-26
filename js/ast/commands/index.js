import { Break } from './break';
import { Return } from './return';
import { Assign } from './assign';
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
// A Proxy to the expression which do what is required. No need to write a new one
import { FunctionCall } from './../expressions/functionCall';

export {
  Break,
  Return,
  Assign,
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
  FunctionCall
};