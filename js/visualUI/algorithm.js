import $ from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals';
import * as VariablesManagement from './variables';
import * as CommandsManagement from './commands';
import * as CodeManagement from './code_generator';
import * as VariableValueMenu from './commands/variable_value_menu';
import * as FunctionsManagement from './functions';
import { DOMConsole } from './../io/domConsole';
import { IVProgParser } from './../ast/ivprogParser';
import { IVProgProcessor } from './../processor/ivprogProcessor';
import { LanguageService } from '../services/languageService';

export function renderAlgorithm () {
	$('.all_functions').empty();

	for (var i = 0; i < window.program_obj.functions.length; i++) {
		FunctionsManagement.renderFunction(window.program_obj.functions[i]);
	}
}