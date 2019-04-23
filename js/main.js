import { runner } from './runner';
import { initVisualUI, addFunctionChangeListener,
  addGlobalChangeListener, removeFunctionListener,
  removeGlobalListener, getTestCases } from './visualUI/functions';
import * as LocalizedStringsService from './services/localizedStringsService';
import { i18nHelper } from "./services/i18nHelper";
import { prepareActivityToStudentHelper, autoEval } from "./util/iassignHelpers";

const i18n = i18nHelper.i18n
const LocalizedStrings = LocalizedStringsService.getInstance();

export {
  runner,
  initVisualUI,
  addFunctionChangeListener,
  addGlobalChangeListener,
  removeFunctionListener,
  removeGlobalListener,
  getTestCases,
  autoEval,
  prepareActivityToStudentHelper,
  LocalizedStrings,
  i18n
}