import { runner } from './runner';
import { initVisualUI, addFunctionChangeListener,
  addGlobalChangeListener, removeFunctionListener,
  removeGlobalListener } from './visualUI/functions';
import * as LocalizedStringsService from './services/localizedStringsService';
import { i18nHelper } from "./services/i18nHelper";

const i18n = i18nHelper.i18n
const LocalizedStrings = LocalizedStringsService.getInstance();

export {
  runner,
  initVisualUI,
  addFunctionChangeListener,
  addGlobalChangeListener,
  removeFunctionListener,
  removeGlobalListener,
  LocalizedStrings,
  i18n
}