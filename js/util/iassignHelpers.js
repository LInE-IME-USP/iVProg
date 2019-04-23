import { setTestCases, getTestCases } from "../visualUI/functions";
import { generate } from "../visualUI/code_generator";
import { IVProgAssessment } from "../assessment/ivprogAssessment";
import { TestConsole } from "./testConsole";

export function prepareActivityToStudentHelper (ilm_cont) {
  const content = JSON.parse(ilm_cont.split('\n::algorithm::')[0]);
  const testCases = content.testcases;
  setTestCases(testCases);
  const settingsDataTypes = content.settings_data_types;
  const settingsCommands = content.settings_commands;
  const settingsFunctions = content.settings_functions;
  let algorithm_in_ilm = null;
  if (ilm_cont.split('\n::algorithm::')[1]) {
      algorithm_in_ilm = ilm_cont.split('\n::algorithm::')[1].split('\n::logs::')[0];
  }
  return {
    settingsDataTypes: settingsDataTypes,
    settingsCommands: settingsCommands,
    settingsFunctions: settingsFunctions,
    algorithmInIlm: algorithm_in_ilm
  }
}

export function autoEval (callback) {
  const code = generate();
  if (code == null) {
    return callback(-1);
  } else {
    const autoAssessment = new IVProgAssessment(code, getTestCases(), new TestConsole([]));
    autoAssessment.runTest().then( grade => callback(grade)).catch(err => console.log(err))
  }
}