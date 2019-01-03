import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { OutputTest } from './../js/util/outputTest';
import { LanguageService } from '../js/services/languageService';

describe('The LanguageService', function () {

  beforeAll( () => {
    
  });

  afterAll(function () {
    
  });

  const code = `program {

    function start() {
      real a = 8.01
      write(a)
    }
  }`;

  const output = new OutputTest();
  LanguageService.setLang('en');
  const lexer = LanguageService.getCurrentLexer();

  it(`should provide the appropriate lexer`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.registerOutput(output);
    exec.interpretAST().then(sto => {
      expect(output.list).toEqual(['8.01']);
      LanguageService.setLang('pt');
      done();
    }).catch( err => done(err));
  });
});
