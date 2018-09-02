import { LocalizedStrings } from '../js/services/localizedStringsService';

describe('The LocalizedStrings services', function () {

  it(`should provide the corret message for the type and id given`, function () {
    expect(LocalizedStrings.getUI('start')).toEqual('inicio');
  });
});
