import line_i18n from "line-i18n";
import { LocalizedStrings } from "./localizedStringsService";

const StringTypes = line_i18n.StringTypes;

export const i18nHelper = Object.freeze({
  i18n: (identifier) => {
    var opts = identifier.split(':');
    var type = opts[0].toLowerCase();
    var id = opts[1];
    if (StringTypes.ERROR === type) {
      return LocalizedStrings.getError(id);
    } else if (StringTypes.MESSAGE === type) {
      return LocalizedStrings.getMessage(id); 
    } else if (StringTypes.UI === type) {
      return LocalizedStrings.getUI(id);
    } else {
      console.warn("A string has been passed to the i18n helper function that was not in the form type:id -> " + identifier);
      return LocalizedStrings.getString(identifier, type);
    }
  }
});