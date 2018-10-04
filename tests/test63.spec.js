import { CompoundType } from "./../js/typeSystem/compoundType";
import { Types } from "./../js/typeSystem/types";

describe("A  CompoundType of an Int and dimension 1", () => {

  it("should not accept another of same dimension", () => {
    const t1 = new CompoundType(Types.INTEGER, 1);
    const t2 = new CompoundType(Types.INTEGER, 1);
    expect(t1.canAccept(t2)).toBeFalsy();
  });
});