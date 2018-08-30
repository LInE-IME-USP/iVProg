import { Types } from './../ast/types';
import { Operators } from './../ast/operators';

const InfixCompatibilityTable = Object.freeze([
  /*Add*/[Types.INTEGER, Types.REAL, Types.STRING],
  /*Sub*/[Types.INTEGER, Types.REAL],
  /*Mult*/[Types.INTEGER, Types.REAL],
  /*Div*/[Types.INTEGER, Types.REAL],
  /*Mod*/[Types.INTEGER],
  /*Gt*/[Types.INTEGER, Types.REAL],
  /*Ge*/[Types.INTEGER, Types.REAL],
  /*Lt*/[Types.INTEGER, Types.REAL],
  /*Le*/[Types.INTEGER, Types.REAL],
  /*Eq*/[Types.INTEGER, Types.REAL, Types.STRING, Types.BOOLEAN],
  /*Neq*/[Types.INTEGER, Types.REAL, Types.STRING, Types.BOOLEAN],
  /*And*/[Types.BOOLEAN],
  /*Or*/[Types.BOOLEAN],
  /*Not*/null,
]);

const UnaryCompatibilityTable = Object.freeze([
  /*Add*/[Types.INTEGER, Types.REAL],
  /*Sub*/[Types.INTEGER, Types.REAL],
  /*Mult*/null,
  /*Div*/null,
  /*Mod*/null,
  /*Gt*/null,
  /*Ge*/null,
  /*Lt*/null,
  /*Le*/null,
  /*Eq*/null,
  /*Neq*/null,
  /*And*/null,
  /*Or*/null,
  /*Not*/[Types.BOOLEAN],
]);

export function canApplyUnaryOp (op, a) {
  const list = UnaryCompatibilityTable[op];
  if (!!!list) {
    return false;
  }
  const type = list.find(t => t === a.type);
  if (!!!type) {
    return false;
  }
  return true
}

export function canApplyInfixOp (op, a, b) {
  const list = InfixCompatibilityTable[op];
  if (!!!list) {
    return false;
  }
  const type = list.find(t => t === a.type);
  if (!!!type) {
    return false;
  }
  return type === b.type;
}