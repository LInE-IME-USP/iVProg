import { LanguageService } from "./languageService";
import line_i18n from 'line-i18n'
import Langs from './../../i18n';
export const StringTypes = line_i18n.StringTypes;
export const LocalizedStrings = Object.freeze(new line_i18n.LocalizedStrings(LanguageService, Langs, true));