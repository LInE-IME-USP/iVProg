import $ from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals_sidebar';
import * as VariablesManagement from './variables';
import * as CommandsManagement from './commands';
import * as CodeManagement from './code_generator';
import * as VariableValueMenu from './commands/variable_value_menu';
import * as FunctionsManagement from './functions_sidebar';
import { DOMConsole } from './../io/domConsole';
import { IVProgParser } from './../ast/ivprogParser';
import { IVProgProcessor } from './../processor/ivprogProcessor';
import { LanguageService } from '../services/languageService';

var block_render = false;

export function renderAlgorithm () {

	if (block_render) {
		return;
	}
	block_render = true;

 	$('.all_functions').children().off();
	$('.all_functions').empty();
	$('.functions_labels').children().off();
	$('.functions_labels').empty();
	$('.list_globals').children().off();
	$('.list_globals').empty();	

	for (var i = 0; i < window.program_obj.functions.length; i++) {
		FunctionsManagement.renderFunction(window.program_obj.functions[i]);
	}

	for (var i = 0; i < window.program_obj.globals.length; i++) {
		GlobalsManagement.renderGlobal(window.program_obj.globals[i]);
	}	

	setTimeout(function(){ block_render = false; }, 500);
}