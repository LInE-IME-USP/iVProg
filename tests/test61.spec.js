import { CompoundType } from "./../js/typeSystem/compoundType";
import { Types } from "./../js/typeSystem/types";

describe("Two CompoundType of an Int and dimension 1", () => {

  it("should be compatible", () => {
    const t1 = new CompoundType(Types.INTEGER, 1);
    const t2 = new CompoundType(Types.INTEGER, 1);
    expect(t1.isCompatible(t2)).toBeTruthy();
  });
});