import { Types } from './../ast/types';
import { Operators } from './../ast/operators';

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
    return infixMap.get(operator)[leftExpressionType.ord][rightExpressionType.ord];
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
    return unaryMap.get(operator)[leftExpressionType.ord];
  } catch (e) {
    if (e instanceof TypeError) {
      return Types.UNDEFINED;
    } else {
      throw e;
    } 
  }
}