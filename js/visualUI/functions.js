import $ from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals';
import * as VariablesManagement from './variables';
import * as CommandsManagement from './commands';
import * as CodeManagement from './code_generator';
import * as VariableValueMenu from './commands/variable_value_menu';
import { DOMConsole } from './../io/domConsole';
import { IVProgParser } from './../ast/ivprogParser';
import { IVProgProcessor } from './../processor/ivprogProcessor';
import WatchJS from 'melanke-watchjs';
import { SemanticAnalyser } from '../processor/semantic/semanticAnalyser';
import { IVProgAssessment } from '../assessment/ivprogAssessment';
import * as AlgorithmManagement from './algorithm';

import '../Sortable.js';

var counter_new_functions = 0;
var counter_new_parameters = 0;

let studentTemp = null;
let domConsole = null;
window.studentGrade = null;
const program = new Models.Program();

window.system_functions = [];
// Adding math functions:
window.system_functions.push(new Models.SystemFunction('$sin', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$cos', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$tan', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$sqrt', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$pow', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true), new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$log', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$abs', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$negate', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$invert', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$max', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
window.system_functions.push(new Models.SystemFunction('$min', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.math));
// Adding text functions:
window.system_functions.push(new Models.SystemFunction('$substring', Types.TEXT, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true),
  new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true),new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.text));
window.system_functions.push(new Models.SystemFunction('$length', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.text));
window.system_functions.push(new Models.SystemFunction('$uppercase', Types.TEXT, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.text));
window.system_functions.push(new Models.SystemFunction('$lowercase', Types.TEXT, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.text));
window.system_functions.push(new Models.SystemFunction('$charAt', Types.TEXT, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true), new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.text));
// Adding arrangement functions:
window.system_functions.push(new Models.SystemFunction('$numElements', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.arrangement));
window.system_functions.push(new Models.SystemFunction('$matrixLines', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.arrangement));
window.system_functions.push(new Models.SystemFunction('$matrixColumns', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.arrangement));
// Adding conversion functions:
window.system_functions.push(new Models.SystemFunction('$isReal', Types.BOOLEAN, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.conversion));
window.system_functions.push(new Models.SystemFunction('$isInt', Types.BOOLEAN, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.conversion));
window.system_functions.push(new Models.SystemFunction('$isBool', Types.BOOLEAN, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.conversion));
window.system_functions.push(new Models.SystemFunction('$castReal', Types.REAL, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.conversion));
window.system_functions.push(new Models.SystemFunction('$castInt', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.conversion));
window.system_functions.push(new Models.SystemFunction('$castBool', Types.BOOLEAN, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.conversion));
window.system_functions.push(new Models.SystemFunction('$castString', Types.TEXT, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.conversion));
/*const variable1 = new Models.Variable(Types.INTEGER, "a", 1);
const parameter1 = new Models.Variable(Types.INTEGER, "par_1", 1);
const command1 = new Models.Comment(new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.only_value, "Testing rendering commands"));

const sumFunction = new Models.Function("soma", Types.INTEGER, 0, [parameter1], false, false, [], null, [command1]);


program.addFunction(sumFunction);
*/

console.log('       ___           ___                    ________          \n      /   /         /   /                  /   ____/  \n     /   /         /   /                  /   /        \n    /   /         /   /  ______    ___   /   /__         \n   /   /         /   /  /      \\  /  /  /   ___/      \n  /   /______   /   /  /   /\\   \\/  /  /   /      \n /          /  /   /  /   /  \\     /  /   /____     \n/__________/  /___/  /___/    \\___/  /________/       ');

const mainFunction = new Models.Function(LocalizedStrings.getUI("start"), Types.VOID, 0, [], true, false);
mainFunction.function_comment = new Models.Comment(LocalizedStrings.getUI('text_comment_main'));
program.addFunction(mainFunction);

window.program_obj = program;

window.generator = CodeManagement.generate;
window.runCodeAssessment = runCodeAssessment;
window.renderAlgorithm = AlgorithmManagement.renderAlgorithm;
window.insertContext = false;

WatchJS.watch(program.globals, function(){
  if (window.insertContext) {
    setTimeout(function(){ AlgorithmManagement.renderAlgorithm(); }, 300);
    window.insertContext = false;
  } else {
    AlgorithmManagement.renderAlgorithm();
  }
}, 1);

function addFunctionHandler () {

	var new_function = new Models.Function(LocalizedStrings.getUI("new_function") + "_" + counter_new_functions, Types.VOID, 0, [], false, false, [], new Models.Comment(LocalizedStrings.getUI('text_comment_start')));
	program.addFunction(new_function);

	counter_new_functions ++;

  renderFunction(new_function);
}

function addParameter (function_obj, function_container) {
  if (function_obj.parameters_list == null) {
    function_obj.parameters_list = [];
  }
  var new_parameter = new Models.Variable(Types.INTEGER, LocalizedStrings.getUI("new_parameter") + "_" + counter_new_parameters);
  function_obj.parameters_list.push(new_parameter);
  counter_new_parameters ++;

  renderParameter(function_obj, new_parameter, function_container);
}

function updateReturnType (function_obj, new_type, new_dimensions = 0) {
  function_obj.return_type = new_type;
  function_obj.return_dimensions = new_dimensions;
}

function removeFunction (function_obj) {
  
  var index = program.functions.indexOf(function_obj);
  if (index > -1) {
    program.functions.splice(index, 1);
  }
}

function minimizeFunction (function_obj) {
  function_obj.is_hidden = !function_obj.is_hidden;
}

function addHandlers (function_obj, function_container) {

  function_container.find('.ui.dropdown.function_return').dropdown({
      onChange: function(value, text, $selectedItem) {
        if ($selectedItem.data('dimensions')) {
          updateReturnType(function_obj, Types[$selectedItem.data('type')], $selectedItem.data('dimensions'));
        } else {
          updateReturnType(function_obj, Types[$selectedItem.data('type')]);
        }
      }
  });

  function_container.find( ".name_function_updated" ).on('click', function(e){
    enableNameFunctionUpdate(function_obj, function_container);
  });

  function_container.find( ".add_parameter_button" ).on('click', function(e){
    addParameter(function_obj, function_container);
  });

  function_container.find('.menu_commands').dropdown({
      on: 'hover'
    });

  function_container.find('.menu_commands a').on('click', function(evt){
    if (function_obj.commands == null || function_obj.commands.length == 0) {
      function_obj.commands = [];
      var new_cmd = CommandsManagement.genericCreateCommand($(this).data('command'));
      function_obj.commands.push(new_cmd);

      CommandsManagement.renderCommand(new_cmd, function_container.find('.commands_list_div'), 3, function_obj);
    } else {
      CommandsManagement.createFloatingCommand(function_obj, function_container, $(this).data('command'), evt);
    }

  });

  function_container.find('.add_var_button_function').on('click', function(e){
    window.insertContext = true;
    VariablesManagement.addVariable(function_obj, function_container, true);
  });

  function_container.find('.remove_function_button').on('click', function(e){
    removeFunction(function_obj);
    function_container.slideUp(400);
  });

  function_container.find('.minimize_function_button').on('click', function(e){
    minimizeFunction(function_obj);
    function_container.find(".function_area").toggle();
    function_container.find(".add_var_top_button").toggle();
  });
}

// Essa função imprime o tipo de retorno da função e cria o menu do tipo 'select' para alteração
function renderFunctionReturn (function_obj, function_element) {

  var ret = '<div class="ui dropdown function_return">';
    
    if (function_obj.return_dimensions > 0) {
      ret += '<div class="text">'+ LocalizedStrings.getUI("vector") +':'+ LocalizedStrings.getUI(function_obj.return_type);
      if (function_obj.return_dimensions == 1) {
        ret += ' [ ] ';
      } else {
        ret += ' [ ] [ ] ';
      }
      ret += '</div>';
    } else {
      ret += '<div class="text">'+LocalizedStrings.getUI(function_obj.return_type)+'</div>';
    }

    ret += '<div class="menu">';


    for (var tm in Types) {
      ret += '<div class="item ' + (function_obj.return_type == tm.toLowerCase()  && function_obj.return_dimensions < 1 ? ' selected ' : '') + '" data-type="'+tm+'" >'+LocalizedStrings.getUI(tm.toLowerCase())+'</div>';
    }

    for (var tm in Types) {
      if (tm == Types.VOID.toUpperCase()) {
        continue;
      }
      ret += '<div class="item">'
        + '<i class="dropdown icon"></i>'
        +  LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())
          +  '<div class="menu">'
            + '<div class="item '+(function_obj.return_type == tm.toLowerCase()  && function_obj.return_dimensions > 0 ? ' selected ' : '')+'" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())+' [ ] " data-type="'+tm+'" data-dimensions="1">[ ]</div>'
            + '<div class="item '+(function_obj.return_type == tm.toLowerCase()  && function_obj.return_dimensions > 0 ? ' selected ' : '')+'" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())+' [ ] [ ] " data-type="'+tm+'" data-dimensions="2">[ ] [ ] </div>'
          +  '</div>'
        + '</div>'; 
    }

    ret += '</div></div>';

    ret = $(ret);
    
    function_element.find('.function_return').append(ret);
}


export function renderFunction (function_obj) {

  var appender = '<div class="ui secondary segment function_div list-group-item">';

  if (function_obj.function_comment) {
    //appender += renderComment(function_obj.function_comment, sequence, true, -1);
  }
    
  appender += '<span class="glyphicon glyphicon-move move_function" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>';

  appender += (function_obj.is_main ? '<div class="div_start_minimize_v"> </div>' : '<button class="ui icon button large remove_function_button"><i class="red icon times"></i></button>')
    + '<button class="ui icon button tiny minimize_function_button"><i class="icon window minimize"></i></button>';

  appender += '<div class="function_signature_div">'+LocalizedStrings.getUI("function")+' ';

  if (function_obj.is_main) {
      appender += '<div class="function_name_div">  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ' + LocalizedStrings.getUI('void') + ' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <span class="span_name_function" >'+function_obj.name+'</span> </div> '
        + ' <span class="parethesis_function">( </span> <div class="ui large labels parameters_list">';
  } else {
      appender += '<div class="ui function_return"></div>';

      appender += '<div class="function_name_div function_name_div_updated"><span class="span_name_function name_function_updated">'+function_obj.name+'</span> </div> ' 
        + ' <span class="parethesis_function"> ( </span> <i class="ui icon plus square outline add_parameter_button"></i> <div class="ui large labels parameters_list container_parameters_list">';
  }
    
  appender += '</div> <span class="parethesis_function"> ) </span> </div>'
    + (function_obj.is_hidden ? ' <div class="function_area" style="display: none;"> ' : ' <div class="function_area"> ');

  appender += '<div class="ui add_var_context add_var_button_function" style="float: left;"><i class="icon plus circle purple"></i><i class="icon circle white back"></i><div class="ui icon button purple"><i class="icon superscript"></i></div></div>';

  appender += '<div class="ui top attached segment variables_list_div"></div>';

  appender += '<div class="ui inline_add_command"><i class="icon plus circle purple"></i><i class="icon circle white back"></i><div class="ui icon button dropdown menu_commands orange" style="float: left;" ><i class="icon code"></i> <div class="menu"> ';
  appender += '<a class="item" data-command="'+Models.COMMAND_TYPES.reader+'"><i class="download icon"></i> ' +LocalizedStrings.getUI('text_read_var')+ '</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.writer+'"><i class="upload icon"></i> '+LocalizedStrings.getUI('text_write_var')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.comment+'"><i class="quote left icon"></i> '+LocalizedStrings.getUI('text_comment')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.attribution+'"><i class="arrow left icon"></i> '+LocalizedStrings.getUI('text_attribution')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.functioncall+'"><i class="hand point right icon"></i> '+LocalizedStrings.getUI('text_functioncall')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.iftrue+'" ><i class="random icon"></i> '+LocalizedStrings.getUI('text_iftrue')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.repeatNtimes+'"><i class="sync icon"></i> '+LocalizedStrings.getUI('text_repeatNtimes')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.whiletrue+'"><i class="sync icon"></i> '+LocalizedStrings.getUI('text_whiletrue')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.dowhiletrue+'"><i class="sync icon"></i> '+LocalizedStrings.getUI('text_dowhiletrue')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.switch+'"><i class="list icon"></i> '+LocalizedStrings.getUI('text_switch')+'</a>'
        + '<a class="item" data-command="'+Models.COMMAND_TYPES.return+'"><i class="reply icon"></i> '+LocalizedStrings.getUI('text_btn_return')+'</a>'
        + '</div></div></div>';

  appender += '<div class="ui bottom attached segment commands_list_div" id="function_drag_cmd_"></div>';

  appender += '</div></div>';

  appender = $(appender);

  $('.all_functions').append(appender);

  appender.data('fun', function_obj);
  appender.find('.commands_list_div').data('fun', function_obj);

  renderFunctionReturn(function_obj, appender);

  addHandlers(function_obj, appender);

  // Rendering parameters: 
  for (var j = 0; j < function_obj.parameters_list.length; j++) {
    renderParameter(function_obj, function_obj.parameters_list[j], appender);
  }

  // Rendering variables:
  for (var j = 0; j < function_obj.variables_list.length; j++) {
    VariablesManagement.renderVariable(appender, function_obj.variables_list[j], function_obj);
  }
  // Rendering commands:
  for (var j = 0; j < function_obj.commands.length; j++) {
    CommandsManagement.renderCommand(function_obj.commands[j], $(appender.find('.commands_list_div')[0]), 3, function_obj);
  }
  $('.minimize_function_button').popup({
    content : LocalizedStrings.getUI("tooltip_minimize"),
    delay: {
      show: 750,
      hide: 0
    }
  });
}

export function initVisualUI () {
  // MUST USE CONST, LET, OR VAR !!!!!!
  const mainDiv = $('#visual-main-div');
  // fill mainDiv with functions and globals...
  // renderAlgorithm()...
  $('.add_function_button').on('click', () => {
    addFunctionHandler();
  });
  $('.add_global_button').on('click', () => {
    window.insertContext = true;
    GlobalsManagement.addGlobal(program, true);
  });

  $('.run_button').on('click', () => {
    runCode();
  });

  $('.visual_coding_button').on('click', () => {
    toggleVisualCoding();
  });

  $('.textual_coding_button').on('click', () => {
    toggleTextualCoding();
  });

  $('.assessment').on('click', () => {
    runCodeAssessment();
    is_iassign = true;
  });

  $('.div_toggle_console').on('click', () => {
    toggleConsole();
  });
}

var is_iassign = false;

$( document ).ready(function() {

  for (var i = 0; i < program.functions.length; i++) {
    renderFunction(program.functions[i]);
  }

  var time_show = 750;
  $('.visual_coding_button').popup({
    content : LocalizedStrings.getUI("tooltip_visual"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.textual_coding_button').popup({
    content : LocalizedStrings.getUI("tooltip_textual"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.upload_file_button').popup({
    content : LocalizedStrings.getUI("tooltip_upload"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.download_file_button').popup({
    content : LocalizedStrings.getUI("tooltip_download"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.undo_button').popup({
    content : LocalizedStrings.getUI("tooltip_undo"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.redo_button').popup({
    content : LocalizedStrings.getUI("tooltip_redo"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.run_button').popup({
    content : LocalizedStrings.getUI("tooltip_run"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.assessment_button').popup({
    content : LocalizedStrings.getUI("tooltip_evaluate"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.help_button').popup({
    content : LocalizedStrings.getUI("tooltip_help"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.add_global_button').popup({
    content : LocalizedStrings.getUI("tooltip_add_global"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.div_toggle_console').popup({
    content : LocalizedStrings.getUI("tooltip_console"),
    delay: {
      show: time_show,
      hide: 0
    }
  });

  Sortable.create(listWithHandle, {
    handle: '.glyphicon-move',
    animation: 100,
    ghostClass: 'ghost',
    group: 'functions_divs_drag',
    onEnd: function (evt) {
       updateSequenceFunction(evt.oldIndex, evt.newIndex);
    }
  });

});

function updateSequenceFunction (oldIndex, newIndex) {
  program_obj.functions.splice(newIndex, 0, program_obj.functions.splice(oldIndex, 1)[0]);
}

function runCodeAssessment () {
  toggleConsole(true);

  window.studentGrade = null;
  studentTemp = null;
  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }
  if(domConsole == null)
    domConsole = new DOMConsole("#ivprog-term");
  $("#ivprog-term").slideDown(500);
  const runner = new IVProgAssessment(strCode, testCases, domConsole);

  runner.runTest().then(grade => {
    if (!is_iassign) {
      parent.getEvaluationCallback(grade);
    } else {
      is_iassign = false;
    }
  }).catch( err => domConsole.err(err.message));
  
}

function runCode () {
  toggleConsole(true);

  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }
  if(domConsole == null)
    domConsole = new DOMConsole("#ivprog-term");
  $("#ivprog-term").slideDown(500);
  try {
    const parser = IVProgParser.createParser(strCode);
    const analyser = new SemanticAnalyser(parser.parseTree());
    const data = analyser.analyseTree();
    const proc = new IVProgProcessor(data);
    proc.registerInput(domConsole);
    proc.registerOutput(domConsole);
    $("#ivprog-term").addClass('ivprog-term-active');

    proc.interpretAST().then( _ => {
      domConsole.info("Programa executado com sucesso!");
      $("#ivprog-term").removeClass('ivprog-term-active');
    }).catch(err => {
      domConsole.err(err.message);
      $("#ivprog-term").removeClass('ivprog-term-active');
    }) 
  } catch (error) {
    domConsole.err(error.message);
    console.log(error);
  }
  
}

function toggleConsole (is_running) {

  if (is_running) {
    $('.ivprog-term-div').css('display', 'block');
    $('#ivprog-term').css('min-height', '160px');
    $('#ivprog-term').css('margin-top', '-170px');
    return;
  }

  if ($('#ivprog-term').css('min-height') == '160px') {
    // esconder
    $('.ivprog-term-div').css('display', 'none');
    $('#ivprog-term').css('min-height', '0');
    $('#ivprog-term').css('margin-top', '-30px');
    $('#ivprog-term').css('padding', '5px');
  } else {
    // mostrar
    $('.ivprog-term-div').css('display', 'block');
    $('#ivprog-term').css('min-height', '160px');
    $('#ivprog-term').css('margin-top', '-170px');
  }
}

function waitToCloseConsole () {
  domConsole.info("Aperte qualquer tecla para fechar...");
  const p = new Promise((resolve, _) => {
    domConsole.requestInput(resolve, true);
  });
  p.then( _ => {
    domConsole.dispose();
    domConsole = null;
    $("#ivprog-term").hide();
  })
}

function toggleTextualCoding () {
  var code = CodeManagement.generate();
  $('.ivprog_visual_panel').css('display', 'none');
  $('.ivprog_textual_panel').css('display', 'block');
  $('.ivprog_textual_panel').removeClass('loading');
  $('.ivprog_textual_code').text(code);

  $('.visual_coding_button').removeClass('active');
  $('.textual_coding_button').addClass('active');
}

function toggleVisualCoding () {
  $('.ivprog_textual_panel').addClass('loading');
  $('.ivprog_textual_panel').css('display', 'none');
  $('.ivprog_visual_panel').css('display', 'block');

  $('.textual_coding_button').removeClass('active');
  $('.visual_coding_button').addClass('active');
}

function removeParameter (function_obj, parameter_obj, parameter_container) {
  var index = function_obj.parameters_list.indexOf(parameter_obj);
  if (index > -1) {
    function_obj.parameters_list.splice(index, 1);
  }
  $(parameter_container).remove();
}

function updateParameterType(parameter_obj, new_type, new_dimensions = 0) {
  parameter_obj.type = new_type;
  parameter_obj.dimensions = new_dimensions;

  if (new_dimensions > 0) {
    parameter_obj.rows = new_dimensions;
    parameter_obj.columns = 2;
  }

}

function renderParameter (function_obj, parameter_obj, function_container) {
  var ret = "";

  ret += '<div class="ui label function_name_parameter">';

  ret += '<div class="ui dropdown parameter_type">';

  if (parameter_obj.dimensions > 0) {
    ret += '<div class="text">'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(parameter_obj.type);
    if (parameter_obj.dimensions == 1) {
      ret += ' [ ] ';
    } else {
      ret += ' [ ] [ ] ';
    }
    ret += '</div>';
  } else {
    ret += '<div class="text">'+LocalizedStrings.getUI(parameter_obj.type)+'</div>';
  }

  ret += '<div class="menu">';

  
  for (var tm in Types) {
      if (tm == Types.VOID.toUpperCase()) {
        continue;
      }
      ret += '<div class="item ' + (parameter_obj.type == tm.toLowerCase() ? ' selected ' : '') + '" data-type="'+tm+'" >'+LocalizedStrings.getUI(tm.toLowerCase())+'</div>';
  }

  for (var tm in Types) {
    if (tm == Types.VOID.toUpperCase()) {
      continue;
    }
    ret += '<div class="item">'
      + '<i class="dropdown icon"></i>'
      +  LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())
        +  '<div class="menu">'
          + '<div class="item" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())+' [ ] " data-type="'+tm+'" data-dimensions="1">[ ]</div>'
          + '<div class="item" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())+' [ ] [ ] " data-type="'+tm+'" data-dimensions="2">[ ] [ ] </div>'
        +  '</div>'
      + '</div>'; 
  }

  ret += '</div></div>';

  ret += '<div class="parameter_div_edit"><span class="span_name_parameter label_enable_name_parameter">'+parameter_obj.name+'</span></div> ';

  ret += ' <i class="red icon times remove_parameter"></i></div>';

  ret = $(ret);
  
  function_container.find('.container_parameters_list').append(ret);

  ret.find('.remove_parameter').on('click', function(e){
    removeParameter(function_obj, parameter_obj, ret);
  });
  
  ret.find('.ui.dropdown.parameter_type').dropdown({
    onChange: function(value, text, $selectedItem) {
      if ($selectedItem.data('dimensions')) {
        updateParameterType(parameter_obj, Types[$selectedItem.data('type')], $selectedItem.data('dimensions'));
      } else {
        updateParameterType(parameter_obj, Types[$selectedItem.data('type')]);
      }
    }
  });

  ret.find('.label_enable_name_parameter').on('click', function(e){
    enableNameParameterUpdate(parameter_obj, ret);
  });

}

var opened_name_parameter = false;
var opened_input_parameter = null;
function enableNameParameterUpdate (parameter_obj, parent_node) {
  if (opened_name_parameter) {
    opened_input_parameter.focus();
    return;
  }
  opened_name_parameter = true;
  parent_node = $(parent_node);

  var input_field;

  parent_node.find('.span_name_parameter').text('');
  input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+parameter_obj.name+"' />" );
  input_field.insertBefore(parent_node.find('.span_name_parameter'));

  input_field.on('input', function() {
    var inputWidth = input_field.textWidth()+10;
    opened_input_parameter = input_field;
    input_field.focus();

    var tmpStr = input_field.val();
    input_field.val('');
    input_field.val(tmpStr);

    input_field.css({
        width: inputWidth
    })
  }).trigger('input');

  input_field.focusout(function() {
    /// update array:
    if (input_field.val().trim()) {
      parameter_obj.name = input_field.val().trim();
      parent_node.find('.span_name_parameter').text(parameter_obj.name);
    }
    input_field.off();
    input_field.remove();

    /// update elements:
    opened_name_parameter = false;
    opened_input_parameter = false;
  });

  input_field.on('keydown', function(e) {
    var code = e.keyCode || e.which;
    if(code == 13) {
      if (input_field.val().trim()) {
        parameter_obj.name = input_field.val().trim();
        parent_node.find('.span_name_parameter').text(parameter_obj.name);
      }
      input_field.off();
      input_field.remove();

      /// update elements:
      opened_name_parameter = false;
      opened_input_parameter = false;

    }
    if(code == 27) {
      parent_node.find('.span_name_parameter').text(parameter_obj.name);
      input_field.off();
      input_field.remove();

      /// update elements:
      opened_name_parameter = false;
      opened_input_parameter = false;
    }
  });
  input_field.select();
}

var opened_name_function = false;
var opened_input = null;
var previousPadding = null;
function enableNameFunctionUpdate (function_obj, parent_node) {
  if (opened_name_function) {
    opened_input.focus();
    return;
  }
  parent_node = $(parent_node);
  parent_node.find('.span_name_function').text('');
  var input_field;
  if (!previousPadding) {
    previousPadding = parent_node.find('.span_name_function').css('padding-left');
  }
  parent_node.find('.span_name_function').css('padding-left', '0');
  parent_node.find('.span_name_function').css('padding-right', '0');
  
  input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+function_obj.name+"' />" );
  input_field.insertBefore(parent_node.find('.span_name_function'));

  input_field.on('input', function() {
    var inputWidth = input_field.textWidth()+10;
    opened_input = input_field;
    input_field.focus();

    var tmpStr = input_field.val();
    input_field.val('');
    input_field.val(tmpStr);

    input_field.css({
        width: inputWidth
    })
  }).trigger('input');

  input_field.focusout(function() {
    /// update array:
    if (input_field.val().trim()) {
      function_obj.name = input_field.val().trim();
    }
    input_field.off();
    input_field.remove();
    parent_node.find('.span_name_function').css('padding-left', previousPadding);
    parent_node.find('.span_name_function').css('padding-right', previousPadding);
    parent_node.find('.span_name_function').text(function_obj.name);

    /// update elements:
    opened_name_function = false;
    opened_input = false;
  });

  input_field.on('keydown', function(e) {
    var code = e.keyCode || e.which;
    if(code == 13) {
      if (input_field.val().trim()) {
        function_obj.name = input_field.val().trim();
      }
      input_field.off();
      input_field.remove();
      parent_node.find('.span_name_function').css('padding-left', previousPadding);
      parent_node.find('.span_name_function').css('padding-right', previousPadding);
      parent_node.find('.span_name_function').text(function_obj.name);

      /// update elements:
      opened_name_function = false;
      opened_input = false;
    }
    if(code == 27) {

      input_field.off();
      input_field.remove();
      parent_node.find('.span_name_function').css('padding-left', previousPadding);
      parent_node.find('.span_name_function').css('padding-right', previousPadding);
      parent_node.find('.span_name_function').text(function_obj.name);

      /// update elements:
      opened_name_function = false;
      opened_input = false;
    }
  });
  input_field.select();
  
}