import { InputTest } from './../js/util/inputTest';

describe('Input test util class', () => {
  const inTest = new InputTest(['0','2']);
  it('should return the values given', (done) => {
    const v1 = new Promise((resolve, _) => {
      inTest.requestInput(resolve);
    });

    const v2 = new Promise((resolve, _) => {
      inTest.requestInput(resolve);
    });

    Promise.all([v1,v2]).then(list => {
      expect(list).toEqual(['0','2']);
      done();
    }).catch(err => done(err));
  });
});