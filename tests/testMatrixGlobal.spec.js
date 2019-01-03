import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { SemanticAnalyser } from "./../js/processor/semantic/semanticAnalyser";
import { LanguageService } from '../js/services/languageService';
import { OutputTest } from '../js/util/outputTest';

describe('A global matrix variable',function () {
  let input = `programa { 
    inteiro nova_global_0 [4][4] = {{1,1,1,1},{1,1,1,1},{1,1,1,1},{1,1,1,1}}

    funcao vazio inicio (  )  {
      escreva ( nova_global_0 [ 0 ]  [ 0 ]  ) 
    }

  }`

  const lexer = LanguageService.getCurrentLexer();
  const out = new OutputTest();

  it(`should not throw an exception`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const semantic = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(semantic.analyseTree());
    exec.registerOutput(out);
    exec.interpretAST().then(_ => {
      expect(out.list.length).toEqual(1);
      done();
    }).catch(err => {
      done(err);
    });
  });
});