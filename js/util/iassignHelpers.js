import { setTestCases, getTestCases } from "../visualUI/functions";
import { generate } from "../visualUI/code_generator";
import { IVProgAssessment } from "../assessment/ivprogAssessment";
import { TestConsole } from "./testConsole";
import { parseLogs } from "./../services/userLog";

function parseActivityData (data) {
  let algorithm_in_ilm = null;
  if (data.split('\n::algorithm::')[1]) {
    algorithm_in_ilm = data.split('\n::algorithm::')[1].split('\n::logs::')[0];
    const logs = data.split('\n::algorithm::')[1].split('\n::logs::')[1];
    if (logs != null) {
      parseLogs(logs);
    }
  }
  let content = JSON.parse(data.split('\n::algorithm::')[0]);
  content['algorithm_in_ilm'] = algorithm_in_ilm;
  return content;
}

export function prepareActivityToStudentHelper (ilm_cont) {
  const content = parseActivityData(ilm_cont);
  const testCases = content.testcases;
  setTestCases(testCases);

  return {
    settingsDataTypes: content.settings_data_types,
    settingsCommands: content.settings_commands,
    settingsFunctions: content.settings_functions,
    algorithmInIlm: content.algorithm_in_ilm
  }
}

export function autoEval (originalData, callback) {
  const code = generate();
  const original = parseActivityData(originalData);
  if (code == null) {
    return callback(-1);
  } else {
    if (!compareTestcases(original.testcases, getTestCases())) {
      return callback(-2);
    }
    const autoAssessment = new IVProgAssessment(code, getTestCases(), new TestConsole([]));
    autoAssessment.runTest().then( grade => callback(grade)).catch(err => {
      console.log(err);
      callback(0);
    });
  }
}

function compareTestcases (original, student) {
  if (original.length != student.length) {
    return false;
  }
  for (let i = 0; i < original.length; ++i) {
    const elementO = original[i];
    const elementS = student[i];
    if(!compareArray(elementO.input, elementS.input)) {
      return false;
    }
    if(!compareArray(elementO.output, elementS.output)) {
      return false;
    }
  }
}

function compareArray (a, b) {
  for (let i = 0; i < a.length; ++i) {
    const elementA = a[i];
    const elementB = b[i];
    if (elementA != elementB) {
      return false;
    }
  }
}