import { resultTypeAfterInfixOp } from './../js/processor/compatibilityTable';
import { Operators } from './../js/ast/operators';
import { Types } from '../js/ast/types';

describe("The compatbility table", () => {
  it("should return the correct type for a given infix operator application", () => {
    const type = resultTypeAfterInfixOp(Operators.ADD, Types.INTEGER, Types.REAL);
    expect(type).toEqual(Types.REAL);
  });
});