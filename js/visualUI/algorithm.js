import * as GlobalsManagement from './globals';
import * as FunctionsManagement from './functions';

window.block_render = false;

export function renderAlgorithm () {
	if (window.block_render) {
		return;
	}
	if (window.draging) {
		return;
	}
	window.block_render = true;
	console.log('rendering algorithm');

 	$('.all_functions').children().off();
	$('.all_functions').empty();

	$('.list_globals').children().off();
	$('.list_globals').empty();	

	for (var i = 0; i < window.program_obj.functions.length; i++) {
		FunctionsManagement.renderFunction(window.program_obj.functions[i]);
	}

	for (var i = 0; i < window.program_obj.globals.length; i++) {
		GlobalsManagement.renderGlobal(window.program_obj.globals[i]);
	}	

	setTimeout(function(){ window.block_render = false; }, 100);
	console.log('fim do render');
}