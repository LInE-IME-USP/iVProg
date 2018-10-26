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

var block_render = false;

export function renderAlgorithm () {

	if (block_render) {
		return;
	}
	block_render = true;

	console.log("rendering algorithm");

	 $('.all_functions').children().off();
	$('.all_functions').empty();

	for (var i = 0; i < window.program_obj.functions.length; i++) {
		FunctionsManagement.renderFunction(window.program_obj.functions[i]);
	}

	setTimeout(function(){ block_render = false; }, 500);
}