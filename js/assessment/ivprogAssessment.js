import { IVProgParser } from "./../ast/ivprogParser";
import { SemanticAnalyser } from "./../processor/semantic/semanticAnalyser";
import { IVProgProcessor } from "./../processor/ivprogProcessor";
import { InputTest } from "./../util/inputTest";
import { OutputTest } from "./../util/outputTest";

export class IVProgAssessment {

  constructor (textCode, testCases, domConsole) {
    this.textCode = textCode;
    this.testCases = testCases;
    this.domConsole = domConsole;
  }

  runTest () {
    return new Promise((resolve, _) => {
      try {
        // try and show error messages through domconsole
        const parser = IVProgParser.createParser(this.textCode);
        const semantic = new SemanticAnalyser(parser.parseTree());
        const processor = new IVProgProcessor(semantic.analyseTree());
        const fun = this.partialBindTestCase(this.evaluateTestCase, processor);
        // loop test cases and show messages through domconsole
        const tests = this.testCases.map( t => fun(t.input, t.output));
        Promise.all(tests).then(results => {
          const count = results.reduce((p, n) => {
            if(n) {
              return p + 1;
            } else {
              return p + 0;
            }
          },0);
          const failed = this.testCases.length - count;
          if(failed === 0) {
            resolve(1);
          } else {
            resolve(count / this.testCases.length);
          }
        }).catch(err => {
          this.domConsole.err("Erro durante a execução do programa");// try and show error messages through domconsole
          this.domConsole.err(err.message);
          resolve(0);
        })
      } catch (error) {
        this.domConsole.err("Erro durante a execução do programa");// try and show error messages through domconsole
        this.domConsole.err(error.message);
        resolve(0);
        return;
      }
    });
  }

  evaluateTestCase (prog, inputList, outputList) {
    return new Promise((resolve, _) => {
      const input = new InputTest(inputList);
      const output = new OutputTest();
      prog.registerInput(input);
      prog.registerOutput(output);
      prog.interpretAST().then( _ => {
        if (input.inputList.length !== 0 ||
          output.list.length !== outputList.length) {
          this.domConsole.err(`Caso de teste ${i + 1} falhou!`);
        } else {
          const isOk = this.checkOutput(output.list, outputList);
          if(!isOk) {
            this.domConsole.err(`Caso de teste ${i + 1} falhou!`);
            resolve(false);
          } else {
            this.domConsole.info(`Caso de teste ${i + 1} passou!`);
            resolve(true);
          }
        }
      })
    });
  }

  partialBindTestCase (fun, param) {
    return (i, o) => fun(param, i, o);
  }

  checkOutput (aList, bList) {
    for (let i = 0; i < aList.length; i++) {
      const outValue = aList[i];
      if(outValue !== bList[i]) {
        return false;
      }
    }
    return true;
  }
}