export const Operators = Object.freeze({
  ADD: {ord: 0, value: "+"},
  SUB: {ord: 1, value: "-"},
  MULT: {ord: 2, value: '*'},
  DIV: {ord: 3, value: '/'},
  MOD: {ord: 4, value: '%'},
  GT: {ord: 5, value: '>'},
  GE: {ord: 6, value: '>='},
  LT: {ord: 7, value: '<'},
  LE: {ord: 8, value: '<='},
  EQ: {ord: 9, value: '=='},
  NEQ: {ord: 10, value: '!='},
  AND: {ord: 11, value: 'and'},
  OR: {ord: 12, value: 'or'},
  NOT: {ord: 13, value: 'not'}
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