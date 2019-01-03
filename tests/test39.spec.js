import { LanguageService } from './../js/services/languageService';
describe('LanguageService.getLang() ', function () {
  LanguageService.setLang('pt_br');
  it("should return the value passed as parameter to LanguageService.setLang()", function () {
    expect(LanguageService.getLang()).toEqual('pt_br');
  });
  afterEach(() => LanguageService.setLang('pt'));
});
