import { LanguageService } from "./languageService";
import Langs from './../../i18n';

export const StringTypes = Object.freeze({
  ERROR: 'error',
  MESSAGE: 'message',
  UI: 'ui'
});

export const LocalizedStrings = Object.freeze({

  getString: (id, type) => {
    let i18nObj = Langs[LanguageService.getLang()];
    if(!!!i18nObj) {
      i18nObj = Langs[LanguageService.getDefaultLang()];
    }
    if(!!!i18nObj[type]) {
      return "{MISSING_I18N_TYPE_IDENTIFIER}";
    } else if (i18nObj[type][id]) {
      return "{MISSING_I18N_TYPE_IDENTIFIER}";
    } else {
      return i18nObj[type][id];
    }
  },

  getError: (id, context = []) => {
    const text = LocalizedStrings.getString(id, StringTypes.ERROR);
    return LocalizedStrings.processString(text, context)
  },

  getMessage: (id, context = []) => {
    const text = LocalizedStrings.getString(id, StringTypes.MESSAGE);
    return LocalizedStrings.processString(text, context)
  },

  getUI: (id, context = []) => {
    const text = LocalizedStrings.getString(id, StringTypes.UI);
    return LocalizedStrings.processString(text, context)
  },

  processString: (text, context) => {
    for (let i = 0; i < context.length; i++) {
      const v = context[i];
      text = text.replace('\$'+i, v);
    }
    return text;
  }

});