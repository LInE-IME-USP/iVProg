import { Decimal } from 'decimal.js';
import line_i18n from 'line-i18n'
import { IVProgParser } from "./../ast/ivprogParser";
import { SemanticAnalyser } from "./../processor/semantic/semanticAnalyser";
import { IVProgProcessor } from "./../processor/ivprogProcessor";
import { InputTest } from "./../util/inputTest";
import { OutputTest } from "./../util/outputTest";
import { DOMConsole} from "./../io/domConsole";
import * as LocalizedStringsService from "../services/localizedStringsService";
import { Config } from "../util/config";


const LocalizedStrings = LocalizedStringsService.getInstance();

const StringTypes = line_i18n.StringTypes;

export class IVProgAssessment {

  constructor (textCode, testCases, domConsole) {
    this.textCode = textCode;
    this.testCases = testCases;
    this.domConsole = domConsole;
  }

  runTest () {
    try {
      const validTree = SemanticAnalyser.analyseFromSource(this.textCode);
      // loop test cases and show messages through domconsole
      const partialTests = this.testCases.map( (t, name) => {
        return this.partialEvaluateTestCase(new IVProgProcessor(validTree), t.input, t.output, name);
      });
      const testResult = partialTests.reduce((acc, curr) => acc.then(curr), Promise.resolve(0));
      return testResult.then(total => {
        const grade = total / this.testCases.length;
        const channel = grade == 1 ? DOMConsole.INFO : DOMConsole.ERR;
        this.writeToConsole(channel, StringTypes.MESSAGE, "test_suite_grade", grade * 100);
        return Promise.resolve(grade)
      }).catch(err => {
          this.domConsole.err("Erro inesperado durante o cálculo da nota.");// try and show error messages through domconsole
          this.domConsole.err(err.message);
          return Promise.resolve(0);
      });
    } catch (error) {
      this.domConsole.err("Erro inesperado durante a execução do programa");// try and show error messages through domconsole
      this.domConsole.err(error.message);
      return Promise.resolve(0);
    }
  }

  evaluateTestCase (prog, inputList, expectedOutputs, name, accumulator) {
    const outerThis = this;
    const input = new InputTest(inputList);
    const output = new OutputTest();
    prog.registerInput(input);
    prog.registerOutput(output);
    const startTime = Date.now()
    return prog.interpretAST().then( _ => {
      const millis = Date.now() - startTime;
      if (input.inputList.length !== input.index) {
        outerThis.showErrorMessage('test_case_few_reads', name+1);
        outerThis.showInfoMessage('test_case_duration', millis);
        return Promise.resolve(accumulator + (input.index/inputList.length));
      } else if (output.list.length != expectedOutputs.length) {
        outerThis.showErrorMessage('test_case_failed', name + 1, inputList.join(','),
          expectedOutputs.join(','), output.list.join(','));
        outerThis.showInfoMessage('test_case_duration', millis);
        // must check for a partial match of the generated output
        const numMatchedOutputs = output.list.reduce((acc, actualOutput, index) => {
          if(outerThis.checkOutputValues(actualOutput, expectedOutputs[index])) {
            return acc + 1;
          } else {
            return acc;
          }
        }, 0);
        const maxLength = Math.max(expectedOutputs.length, output.list.length);
        return Promise.resolve(accumulator + (numMatchedOutputs/maxLength));
      } else {
        const isOk = outerThis.checkOutputLists(output.list, expectedOutputs);
        if(!isOk) {
          console.log("not ok.");
          outerThis.showErrorMessage('test_case_failed', name + 1, inputList.join(','),
            expectedOutputs.join(','), output.list.join(','));
          outerThis.showInfoMessage('test_case_duration', millis);
          return Promise.resolve(accumulator);
        } else {
          outerThis.showInfoMessage('test_case_success', name + 1);
          outerThis.showInfoMessage('test_case_duration', millis);
          return Promise.resolve(accumulator + 1);
        }
      }
    }).catch( error => {
      outerThis.showErrorMessage('test_case_failed_exception', name + 1, error.message);
      return Promise.resolve(accumulator);
    });
  }

  partialEvaluateTestCase (prog, inputList, expectedOutputs, name) {
    return this.evaluateTestCase.bind(this, prog, inputList, expectedOutputs, name);
  }

  checkOutputLists (actualOutputs, expectedOutputs) {
    for (let i = 0; i < actualOutputs.length; i++) {
      const outValue = actualOutputs[i];
      const expectedValue = expectedOutputs[i];
      if(!this.checkOutputValues(outValue, expectedValue)) {
        return false;
      }
    }
    return true;
  }

  checkOutputValues (actualValue, expectedValue) {
    let castNumberA = parseFloat(actualValue);
    if(!Number.isNaN(castNumberA)) {
      let castNumberB = parseFloat(expectedValue);
      if(Number.isNaN(castNumberB)) {
        return false;
      }
      castNumberA = new Decimal(castNumberA);
      castNumberB = new Decimal(castNumberB);
      const decimalPlaces = Math.min(castNumberB.dp(), Config.decimalPlaces);
      Decimal.set({ rounding: Decimal.ROUND_FLOOR});
      castNumberA = new Decimal(castNumberA.toFixed(decimalPlaces));
      castNumberB = new Decimal(castNumberB.toFixed(decimalPlaces));
      const aEqualsB = castNumberA.eq(castNumberB);
      Decimal.set({ rounding: Decimal.ROUND_HALF_UP});
      if (!aEqualsB) {
        return false;
      }
    } else if(actualValue != expectedValue) {
      return false;
    }
    return true;
  }

  showErrorMessage (errorID, ...args) {
    this.domConsole.err(LocalizedStrings.getError(errorID, args));
  }

  showInfoMessage (msgID, ...args) {
    this.domConsole.info(LocalizedStrings.getMessage(msgID, args));
  }

  writeToConsole (channel, msgType, msgID, ...args) {
    let msg = LocalizedStrings.getString(msgID, msgType);
    msg = LocalizedStrings.processString(msg, args);
    switch(channel) {
      case DOMConsole.ERR: {
        this.domConsole.err(msg);
        break;
      }
      case DOMConsole.INFO: {
        this.domConsole.info(msg);
        break;
      }
      case DOMConsole.USER: {
        this.domConsole.write(msg);
        break;
      }
    }
  }
}