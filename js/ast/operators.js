export const Operators = Object.freeze({
  ADD: 0,
  SUB: 1,
  MULT: 2,
  DIV: 3,
  MOD: 4,
  GT: 5,
  GE: 6,
  LT: 7,
  LE: 8,
  EQ: 9,
  NEQ: 10,
  AND: 11,
  OR: 12,
  NOT: 13
});

export function convertFromString (op) {
  switch (op) {
    case '+' : return Operators.ADD;
    case '-' : return Operators.SUB;
    case '*' : return Operators.MULT;
    case '/' : return Operators.DIV;
    case '%' : return Operators.MOD;
    case '>' : return Operators.GT;
    case '>=' : return Operators.GE;
    case '<' : return Operators.LT;
    case '<=' : return Operators.LE;
    case '==' : return Operators.EQ;
    case '!=' : return Operators.NEQ;
    case 'and' : return Operators.AND;
    case 'or' : return Operators.OR;
    case 'not' : return Operators.NOT;
  }
}