import { LanguageService } from './../js/services/languageService';
describe('LanguageService.getLang() ', function () {
  beforeEach(() => {
    localStorage.setItem('ivprog.lang', 'pt_br');
  });
  it("should return the value associated to the key ivprog.lang, inside localStorage", function () {
    expect(LanguageService.getLang()).toEqual('pt_br');
  });
  afterEach(() => localStorage.removeItem('ivprog.lang'));
});
