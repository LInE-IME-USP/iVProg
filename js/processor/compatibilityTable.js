import { Types } from './../typeSystem/types';
import { Operators } from './../ast/operators';
import { MultiType } from '../typeSystem/multiType';
import { Config } from '../util/config';

function buildInfixAddTable () {
  const table = [[], [], [], []];

  table[Types.INTEGER.ord][Types.INTEGER.ord] = Types.INTEGER;
  table[Types.INTEGER.ord][Types.REAL.ord] = Types.REAL;
  table[Types.INTEGER.ord][Types.STRING.ord] = Types.STRING;

  table[Types.REAL.ord][Types.INTEGER.ord] = Types.REAL;
  table[Types.REAL.ord][Types.REAL.ord] = Types.REAL;
  table[Types.REAL.ord][Types.STRING.ord] = Types.STRING;

  table[Types.STRING.ord][Types.INTEGER.ord] = Types.STRING;
  table[Types.STRING.ord][Types.REAL.ord] = Types.STRING;
  table[Types.STRING.ord][Types.STRING.ord] = Types.STRING;
  table[Types.STRING.ord][Types.BOOLEAN.ord] = Types.STRING;

  return table;
}

function buildInfixMultiDivSubTable () {
  const table = [[], [], [], []];

  table[Types.INTEGER.ord][Types.INTEGER.ord] = Types.INTEGER;
  table[Types.INTEGER.ord][Types.REAL.ord] = Types.REAL;

  table[Types.REAL.ord][Types.INTEGER.ord] = Types.REAL;
  table[Types.REAL.ord][Types.REAL.ord] = Types.REAL;

  return table;
}

function buildInfixEqualityInequalityTable () {
  const table = [[], [], [], []];

  table[Types.INTEGER.ord][Types.INTEGER.ord] = Types.BOOLEAN;

  table[Types.REAL.ord][Types.REAL.ord] = Types.BOOLEAN;

  table[Types.BOOLEAN.ord][Types.BOOLEAN.ord] = Types.BOOLEAN;

  table[Types.STRING.ord][Types.STRING.ord] = Types.BOOLEAN;

  return table;
}

function buildInfixRelationalTable () {
  const table = [[], [], [], []];

  table[Types.INTEGER.ord][Types.INTEGER.ord] = Types.BOOLEAN;

  table[Types.REAL.ord][Types.REAL.ord] = Types.BOOLEAN;

  table[Types.STRING.ord][Types.STRING.ord] = Types.BOOLEAN;

  return table;
}

function buildInfixAndOrTable () {
  const table = [[], [], [], []];

  table[Types.BOOLEAN.ord][Types.BOOLEAN.ord] = Types.BOOLEAN;

  return table;
}

function buildInfixModTable () {
  const table = [[], [], [], []];

  table[Types.INTEGER.ord][Types.INTEGER.ord] = Types.INTEGER;

  return table;
}

function buildUnarySumSubList () {
  const list = [];

  list[Types.INTEGER.ord] = Types.INTEGER;

  list[Types.REAL.ord] = Types.REAL;

  return list;
}

function buildUnaryNegList () {
  const list = [];

  list[Types.BOOLEAN.ord] = Types.BOOLEAN;

  return list;
}

function buildInfixCompatibilityTable () {
  const compatibilityMap = new WeakMap();
  compatibilityMap.set(Operators.ADD, buildInfixAddTable());
  compatibilityMap.set(Operators.SUB, buildInfixMultiDivSubTable());
  compatibilityMap.set(Operators.MULT, buildInfixMultiDivSubTable());
  compatibilityMap.set(Operators.DIV, buildInfixMultiDivSubTable());
  compatibilityMap.set(Operators.EQ, buildInfixEqualityInequalityTable());
  compatibilityMap.set(Operators.NEQ, buildInfixEqualityInequalityTable());
  compatibilityMap.set(Operators.GE, buildInfixRelationalTable());
  compatibilityMap.set(Operators.GT, buildInfixRelationalTable());
  compatibilityMap.set(Operators.LE, buildInfixRelationalTable());
  compatibilityMap.set(Operators.LT, buildInfixRelationalTable());
  compatibilityMap.set(Operators.OR, buildInfixAndOrTable());
  compatibilityMap.set(Operators.AND, buildInfixAndOrTable());
  compatibilityMap.set(Operators.MOD, buildInfixModTable());
  return compatibilityMap;
}

function buildUnaryCompatibilityTable () {
  const compatibilityMap = new WeakMap();
  compatibilityMap.set(Operators.ADD, buildUnarySumSubList());
  compatibilityMap.set(Operators.SUB, buildUnarySumSubList());
  compatibilityMap.set(Operators.NOT, buildUnaryNegList());
  return compatibilityMap;
}

const infixMap = buildInfixCompatibilityTable();
const unaryMap = buildUnaryCompatibilityTable();

export function resultTypeAfterInfixOp (operator, leftExpressionType, rightExpressionType) {
  try {
    if(leftExpressionType instanceof MultiType && rightExpressionType instanceof MultiType) {
      let newMulti = [];
      for (let i = 0; i < leftExpressionType.types.length; ++i) {
        const typeA = leftExpressionType.types[i];
        for(let j = 0; j < rightExpressionType.types.length; ++i) {
          const typeB = rightExpressionType.types[j];
          newMulti.push(resultTypeAfterInfixOp(operator, typeA, typeB));
        }
      }
      newMulti = newMulti.filter(x => !x.isCompatible(Types.UNDEFINED));
      if(newMulti.length <= 0) {
        if(Config.enable_type_casting) {
          if(leftExpressionType.isCompatible(Types.INTEGER) || leftExpressionType.isCompatible(Types.REAL)) {
            if(rightExpressionType.isCompatible(Types.INTEGER) || rightExpressionType.isCompatible(Types.REAL)) {
              return new MultiType([Types.INTEGER, Types.REAL]);
            }
          }
        }
        return Types.UNDEFINED;
      } else {
        return new MultiType(newMulti)
      }
    } else if(leftExpressionType instanceof MultiType) {
      if(leftExpressionType.isCompatible(rightExpressionType)) {
        return resultTypeAfterInfixOp(operator, rightExpressionType, rightExpressionType);
      } else {
        if(Config.enable_type_casting) {
          if(leftExpressionType.isCompatible(Types.INTEGER) || leftExpressionType.isCompatible(Types.REAL)) {
            if(rightExpressionType.isCompatible(Types.INTEGER) || rightExpressionType.isCompatible(Types.REAL)) {
              return rightExpressionType;
            }
          }
        }
        return Types.UNDEFINED;
      }
    } else if(rightExpressionType instanceof MultiType) {
      if(rightExpressionType.isCompatible(leftExpressionType)) {
        return resultTypeAfterInfixOp(operator, leftExpressionType, leftExpressionType);
      } else {
        if(Config.enable_type_casting) {
          if(leftExpressionType.isCompatible(Types.INTEGER) || leftExpressionType.isCompatible(Types.REAL)) {
            if(rightExpressionType.isCompatible(Types.INTEGER) || rightExpressionType.isCompatible(Types.REAL)) {
              return leftExpressionType;
            }
          }
        }
        return Types.UNDEFINED;
      }
    }
    const resultType = infixMap.get(operator)[leftExpressionType.ord][rightExpressionType.ord];
    if (resultType === null || resultType === undefined) {
      if(Config.enable_type_casting) {
        if(leftExpressionType.isCompatible(Types.INTEGER) || leftExpressionType.isCompatible(Types.REAL)) {
          if(rightExpressionType.isCompatible(Types.INTEGER) || rightExpressionType.isCompatible(Types.REAL)) {
            if(operator === Operators.MOD) {
              return Types.INTEGER;
            } else if (operator.ord >= 5 && operator.ord <= 10){
              return Types.BOOLEAN;
            }
          }
        }
      }
      return Types.UNDEFINED
    }
    return resultType;
  } catch (e) {
    if (e instanceof TypeError) {
      return Types.UNDEFINED;
    } else {
      throw e;
    }
  }
}

export function resultTypeAfterUnaryOp (operator, leftExpressionType) {
  try {
    if(leftExpressionType instanceof MultiType){
      return leftExpressionType;
    }
    return unaryMap.get(operator)[leftExpressionType.ord];
  } catch (e) {
    if (e instanceof TypeError) {
      return Types.UNDEFINED;
    } else {
      throw e;
    } 
  }
}