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
    const outerRef = this;
    return new Promise((resolve, _) => {
      try {
        // try and show error messages through domconsole
        const parser = IVProgParser.createParser(outerRef.textCode);
        const semantic = new SemanticAnalyser(parser.parseTree());
        const validTree = semantic.analyseTree();
        // loop test cases and show messages through domconsole
        const tests = outerRef.testCases.map( (t, name) => {
          return outerRef.evaluateTestCase(new IVProgProcessor(validTree), t.input, t.output, name);
        });
        Promise.all(tests).then(results => {
          const count = results.reduce((lastValue, nextValue) =>  lastValue + nextValue, 0);
          resolve(count / outerRef.testCases.length);
        }).catch(err => {
          outerRef.domConsole.err("Erro durante a execução do programa");// try and show error messages through domconsole
          outerRef.domConsole.err(err.message);
          resolve(0);
        })
      } catch (error) {
        outerRef.domConsole.err("Erro durante a execução do programa");// try and show error messages through domconsole
        outerRef.domConsole.err(error.message);
        resolve(0);
      }
    });
  }

  evaluateTestCase (prog, inputList, outputList, name) {
    const outerThis = this;
    return new Promise((resolve, reject) => {
      const input = new InputTest(inputList);
      const output = new OutputTest();
      prog.registerInput(input);
      prog.registerOutput(output);
      const startTime = Date.now()
      prog.interpretAST().then( _ => {
        const millis = Date.now() - startTime;
        if (input.inputList.length !== input.index) {
          outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou, ainda restam entradas!`);
          outerThis.domConsole.err(`Levou ${millis}ms`);
          resolve(1 * (input.index/inputList.length));
        } else if (output.list.length < outputList.length) {
          outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou <${inputList.join(", ")};${outputList.join(", ")};${output.list.join(", ")}>`);
          outerThis.domConsole.err(`Levou ${millis}ms`);
          resolve(1 * (output.list.length/outputList.length));
        } else if (output.list.length > outputList.length) {
          outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou <${inputList.join(", ")};${outputList.join(", ")};${output.list.join(", ")}>`);
          outerThis.domConsole.err(`Levou ${millis}ms`);
          resolve(1 * (outputList.length/output.list.length));
        } else {
          const isOk = outerThis.checkOutput(output.list, outputList);
          if(!isOk) {
            outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou <${inputList.join(", ")};${outputList.join(", ")};${output.list.join(", ")}>`);
            outerThis.domConsole.err(`Levou ${millis}ms`);
            resolve(0);
          } else {
            outerThis.domConsole.info(`Caso de teste ${name + 1}: OK!`);
            outerThis.domConsole.err(`Levou ${millis}ms`);
            resolve(1);
          }
        }
      }).catch( _ => resolve(0));
    })
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