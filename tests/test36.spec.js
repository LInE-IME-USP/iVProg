import { OutputTest } from './../js/util/outputTest';

describe('Output test util class', () => {
  it('should store the values it received', () => {
    const values = ['1','2'];

    const out = new OutputTest();
    values.forEach( v => out.sendOutput(v));
    expect(out.list).toEqual(['1','2']);
  });
});