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
    try {
      // try and show error messages through domconsole
      const parser = IVProgParser.createParser(this.textCode);
      const semantic = new SemanticAnalyser(parser.parseTree());
      const validTree = semantic.analyseTree();
      // loop test cases and show messages through domconsole
      const partialTests = this.testCases.map( (t, name) => {
        return this.partialEvaluateTestCase(new IVProgProcessor(validTree), t.input, t.output, name);
      });
      const testResult = partialTests.reduce((acc, curr) => acc.then(curr), Promise.resolve(0));
      return testResult.then(total => Promise.resolve(total / this.testCases.length))
        .catch(err => {
          this.domConsole.err("Erro durante a execução do programa");// try and show error messages through domconsole
          this.domConsole.err(err.message);
          return Promise.resolve(0);
      });
    } catch (error) {
      this.domConsole.err("Erro durante a execução do programa");// try and show error messages through domconsole
      this.domConsole.err(error.message);
      return Promise.resolve(0);
    }
  }

  evaluateTestCase (prog, inputList, outputList, name, accumulator) {
    const outerThis = this;
    const input = new InputTest(inputList);
    const output = new OutputTest();
    prog.registerInput(input);
    prog.registerOutput(output);
    const startTime = Date.now()
    return prog.interpretAST().then( _ => {
      const millis = Date.now() - startTime;
      if (input.inputList.length !== input.index) {
        outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou, ainda restam entradas!`);
        outerThis.domConsole.info(`Levou ${millis}ms`);
        return Promise.resolve(accumulator + 1 * (input.index/inputList.length));
      } else if (output.list.length < outputList.length) {
        outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou <${inputList.join(", ")};${outputList.join(", ")};${output.list.join(", ")}>`);
        outerThis.domConsole.info(`Levou ${millis}ms`);
        return Promise.resolve(accumulator + 1 * (output.list.length/outputList.length));
      } else if (output.list.length > outputList.length) {
        outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou <${inputList.join(", ")};${outputList.join(", ")};${output.list.join(", ")}>`);
        outerThis.domConsole.info(`Levou ${millis}ms`);
        return Promise.resolve(accumulator + 1 * (outputList.length/output.list.length));
      } else {
        const isOk = outerThis.checkOutput(output.list, outputList);
        if(!isOk) {
          outerThis.domConsole.err(`Caso de teste ${name + 1}: Falhou <${inputList.join(", ")};${outputList.join(", ")};${output.list.join(", ")}>`);
          outerThis.domConsole.info(`Levou ${millis}ms`);
          return Promise.resolve(accumulator);
        } else {
          outerThis.domConsole.info(`Caso de teste ${name + 1}: OK!`);
          outerThis.domConsole.info(`Levou ${millis}ms`);
          return Promise.resolve(accumulator + 1);
        }
      }
    }).catch( error => {
      this.domConsole.err(`Execução do caso de teste ${name + 1} falhou!`);// try and show error messages through domconsole
      this.domConsole.err(error.message);
      return Promise.resolve(accumulator);
    });
  }

  partialEvaluateTestCase (prog, inputList, outputList, name) {
    return this.evaluateTestCase.bind(this, prog, inputList, outputList, name);
  }

  checkOutput (aList, bList) {
    for (let i = 0; i < aList.length; i++) {
      const outValue = aList[i];
      if(outValue != bList[i]) {
        return false;
      }
    }
    return true;
  }
}