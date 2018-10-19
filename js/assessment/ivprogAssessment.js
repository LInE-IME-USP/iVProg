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
    let success = 0;
    try {
      // try and show error messages through domconsole
      const parser = IVProgParser.createParser(this.textCode);
      const semantic = new SemanticAnalyser(parser.parseTree());
      const processor = new IVProgProcessor(semantic.analyseTree());
      // loop test cases and show messages through domconsole
      for (let i = 0; i < this.testCases.length; i++) {
        const testCase = this.testCases[i];
        const input = new InputTest(testCase.input);
        const output = new OutputTest();
        processor.registerInput(input);
        processor.registerOutput(output);
        processor.interpretAST();
        if (input.inputList.length !== 0 ||
          output.list.length !== testCase.output.length) {
          this.domConsole.err(`Caso de teste ${i+1} falhou!`);
        } else {
          const isOk = this.checkOutput(output.list, testCase.output);
          if(!isOk) {
            this.domConsole.err(`Caso de teste ${i+1} falhou!`);
          } else {
            this.domConsole.info(`Caso de teste ${i+1} passou!`);
            success++;
          }
        }
      } 
    } catch (error) {
      this.domConsole.err("Erro durante a execução do programa");// try and show error messages through domconsole
      this.domConsole.err(error.message);
      return 0;
    }
    const failed = this.testCases.length - success;
    if(failed === 0) {
      return 1;
    } else {
      return success / this.testCases.length;
    }
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