import $ from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements_sidebar';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals_sidebar';
import * as VariablesManagement from './variables';
import * as CommandsManagement from './commands_sidebar';
import * as CodeManagement from './code_generator';
import * as VariableValueMenu from './commands/variable_value_menu';
import { DOMConsole } from './../io/domConsole';
import { IVProgParser } from './../ast/ivprogParser';
import { IVProgProcessor } from './../processor/ivprogProcessor';
import WatchJS from 'melanke-watchjs';
import { SemanticAnalyser } from '../processor/semantic/semanticAnalyser';
import { IVProgAssessment } from '../assessment/ivprogAssessment';
import * as AlgorithmManagement from './algorithm_sidebar';

import '../Sortable.js';

var counter_new_functions = 0;
var counter_new_parameters = 0;

let studentTemp = null;
let domConsole = null;
window.studentGrade = null;
const program = new Models.Program();

window.system_functions = [];
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

WatchJS.watch(program.globals, function () {
  AlgorithmManagement.renderAlgorithm();
}, 1);

function addFunctionHandler() {

  var new_function = new Models.Function(LocalizedStrings.getUI("new_function") + "_" + counter_new_functions, Types.VOID, 0, [], false, false, [], new Models.Comment(LocalizedStrings.getUI('text_comment_start')));
  program.addFunction(new_function);

  counter_new_functions++;

  renderFunction(new_function);
}

function addParameter(function_obj, function_container) {
  if (function_obj.parameters_list == null) {
    function_obj.parameters_list = [];
  }
  var new_parameter = new Models.Variable(Types.INTEGER, LocalizedStrings.getUI("new_parameter") + "_" + counter_new_parameters);
  function_obj.parameters_list.push(new_parameter);
  counter_new_parameters++;

  renderParameter(function_obj, new_parameter, function_container);
}

function updateReturnType(function_obj, new_type, new_dimensions = 0) {
  function_obj.return_type = new_type;
  function_obj.return_dimensions = new_dimensions;
}

function removeFunction(function_obj) {

  var index = program.functions.indexOf(function_obj);
  if (index > -1) {
    program.functions.splice(index, 1);
  }
}

function minimizeFunction(function_obj) {
  function_obj.is_hidden = !function_obj.is_hidden;
}

function addHandlers(function_obj, function_container) {

  function_container.find('.ui.dropdown.function_return').dropdown({
    onChange: function (value, text, $selectedItem) {
      if ($selectedItem.data('dimensions')) {
        updateReturnType(function_obj, Types[$selectedItem.data('type')], $selectedItem.data('dimensions'));
      } else {
        updateReturnType(function_obj, Types[$selectedItem.data('type')]);
      }
    }
  });

  function_container.find(".name_function_updated").on('click', function (e) {
    enableNameFunctionUpdate(function_obj, function_container);
  });

  function_container.find(".add_parameter_button").on('click', function (e) {
    addParameter(function_obj, function_container);
  });

  function_container.find('.menu_commands').dropdown({
    on: 'hover'
  });

  function_container.find('.menu_commands a').on('click', function (evt) {
    if (function_obj.commands == null || function_obj.commands.length == 0) {
      function_obj.commands = [];
      var new_cmd = CommandsManagement.genericCreateCommand($(this).data('command'));
      function_obj.commands.push(new_cmd);

      CommandsManagement.renderCommand(new_cmd, function_container.find('.commands_list_div'), 3, function_obj);
    } else {
      CommandsManagement.createFloatingCommand(function_obj, function_container, $(this).data('command'), evt);
    }

  });

  function_container.find('.add_var_button_function').on('click', function (e) {
    VariablesManagement.addVariable(function_obj, function_container);
  });

  function_container.find('.remove_function_button').on('click', function (e) {
    removeFunction(function_obj);
    function_container.slideUp(400);
  });

  function_container.find('.minimize_function_button').on('click', function (e) {
    minimizeFunction(function_obj);
    function_container.find(".function_area").toggle();
    function_container.find(".add_var_top_button").toggle();
  });
}

// Essa função imprime o tipo de retorno da função e cria o menu do tipo 'select' para alteração
function renderFunctionReturn(function_obj, function_element) {

  var ret = '<div class="ui dropdown function_return">';

  if (function_obj.return_dimensions > 0) {
    ret += '<div class="text">' + LocalizedStrings.getUI("vector") + ':' + LocalizedStrings.getUI(function_obj.return_type);
    if (function_obj.return_dimensions == 1) {
      ret += ' [ ] ';
    } else {
      ret += ' [ ] [ ] ';
    }
    ret += '</div>';
  } else {
    ret += '<div class="text">' + LocalizedStrings.getUI(function_obj.return_type) + '</div>';
  }

  ret += '<div class="menu">';


  for (var tm in Types) {
    ret += '<div class="item ' + (function_obj.return_type == tm.toLowerCase() && function_obj.return_dimensions < 1 ? ' selected ' : '') + '" data-type="' + tm + '" >' + LocalizedStrings.getUI(tm.toLowerCase()) + '</div>';
  }

  for (var tm in Types) {
    if (tm == Types.VOID.toUpperCase()) {
      continue;
    }
    ret += '<div class="item">'
      + '<i class="dropdown icon"></i>'
      + LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(tm.toLowerCase())
      + '<div class="menu">'
      + '<div class="item ' + (function_obj.return_type == tm.toLowerCase() && function_obj.return_dimensions > 0 ? ' selected ' : '') + '" data-text="' + LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(tm.toLowerCase()) + ' [ ] " data-type="' + tm + '" data-dimensions="1">[ ]</div>'
      + '<div class="item ' + (function_obj.return_type == tm.toLowerCase() && function_obj.return_dimensions > 0 ? ' selected ' : '') + '" data-text="' + LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(tm.toLowerCase()) + ' [ ] [ ] " data-type="' + tm + '" data-dimensions="2">[ ] [ ] </div>'
      + '</div>'
      + '</div>';
  }

  ret += '</div></div>';

  ret = $(ret);

  function_element.find('.function_return').append(ret);
}


export function renderFunction(function_obj) {

  var appender = '<div class="ui secondary segment function_div list-group-item">';

  if (function_obj.function_comment) {
    //appender += renderComment(function_obj.function_comment, sequence, true, -1);
  }

  appender += '<span class="glyphicon glyphicon-move move_function" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>';

  appender += (function_obj.is_main ? '<div class="div_start_minimize_v"> </div>' : '<button class="ui icon button large remove_function_button"><i class="red icon times"></i></button>')
    + '<button class="ui icon button tiny minimize_function_button"><i class="icon window minimize"></i></button>';

  appender += '<div class="ui small icon buttons add_var_top_button"><div class="ui icon button add_var_button_function"><i class="icon superscript"></i></div>';

  appender += '<div class="ui icon button dropdown menu_commands" ><i class="icon code"></i> <div class="menu"> ';
  appender += '<a class="item" data-command="' + Models.COMMAND_TYPES.reader + '"><i class="download icon"></i> ' + LocalizedStrings.getUI('text_read_var') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.writer + '"><i class="upload icon"></i> ' + LocalizedStrings.getUI('text_write_var') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.comment + '"><i class="quote left icon"></i> ' + LocalizedStrings.getUI('text_comment') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.attribution + '"><i class="arrow left icon"></i> ' + LocalizedStrings.getUI('text_attribution') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.functioncall + '"><i class="hand point right icon"></i> ' + LocalizedStrings.getUI('text_functioncall') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.iftrue + '" ><i class="random icon"></i> ' + LocalizedStrings.getUI('text_iftrue') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.repeatNtimes + '"><i class="sync icon"></i> ' + LocalizedStrings.getUI('text_repeatNtimes') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.whiletrue + '"><i class="sync icon"></i> ' + LocalizedStrings.getUI('text_whiletrue') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.dowhiletrue + '"><i class="sync icon"></i> ' + LocalizedStrings.getUI('text_dowhiletrue') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.switch + '"><i class="list icon"></i> ' + LocalizedStrings.getUI('text_switch') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.return + '"><i class="reply icon"></i> ' + LocalizedStrings.getUI('text_btn_return') + '</a>'
    + '</div></div></div>';

  appender += '<div class="function_signature_div">' + LocalizedStrings.getUI("function") + ' ';

  if (function_obj.is_main) {
    appender += '<div class="function_name_div">  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ' + LocalizedStrings.getUI('void') + ' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <span class="span_name_function" >' + function_obj.name + '</span> </div> '
      + ' <span class="parethesis_function">( </span> <div class="ui large labels parameters_list">';
  } else {
    appender += '<div class="ui function_return"></div>';

    appender += '<div class="function_name_div function_name_div_updated"><span class="span_name_function name_function_updated">' + function_obj.name + '</span> </div> '
      + ' <span class="parethesis_function"> ( </span> <i class="ui icon plus square outline add_parameter_button"></i> <div class="ui large labels parameters_list container_parameters_list">';
  }

  appender += '</div> <span class="parethesis_function"> ) </span> </div>'
    + (function_obj.is_hidden ? ' <div class="function_area" style="display: none;"> ' : ' <div class="function_area"> ')

    + '<div class="ui top attached segment variables_list_div">'
    + '</div>'

    + '<div class="ui bottom attached segment commands_list_div" id="function_drag_cmd_">';

  appender += '</div>';

  appender += '<div class="function_close_div"></div>'
    + '</div>'
    + '</div>';

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
    content: LocalizedStrings.getUI("tooltip_minimize"),
    delay: {
      show: 750,
      hide: 0
    }
  });
}


export function initVisualUI() {
  // MUST USE CONST, LET, OR VAR !!!!!!
  const mainDiv = $('#visual-main-div');
  // fill mainDiv with functions and globals...
  // renderAlgorithm()...
  $('.add_function_button').on('click', () => {
    addFunctionHandler();
  });
  $('.add_global_button').on('click', () => {
    GlobalsManagement.addGlobal(program);
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

$(document).ready(function () {

  for (var i = 0; i < program.functions.length; i++) {
    renderFunction(program.functions[i]);
  }

  var time_show = 750;
  $('.visual_coding_button').popup({
    content: LocalizedStrings.getUI("tooltip_visual"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.textual_coding_button').popup({
    content: LocalizedStrings.getUI("tooltip_textual"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.upload_file_button').popup({
    content: LocalizedStrings.getUI("tooltip_upload"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.download_file_button').popup({
    content: LocalizedStrings.getUI("tooltip_download"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.undo_button').popup({
    content: LocalizedStrings.getUI("tooltip_undo"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.redo_button').popup({
    content: LocalizedStrings.getUI("tooltip_redo"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.run_button').popup({
    content: LocalizedStrings.getUI("tooltip_run"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.assessment_button').popup({
    content: LocalizedStrings.getUI("tooltip_evaluate"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.help_button').popup({
    content: LocalizedStrings.getUI("tooltip_help"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.add_global_button').popup({
    content: LocalizedStrings.getUI("tooltip_add_global"),
    delay: {
      show: time_show,
      hide: 0
    }
  });
  $('.div_toggle_console').popup({
    content: LocalizedStrings.getUI("tooltip_console"),
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

function updateSequenceFunction(oldIndex, newIndex) {
  program_obj.functions.splice(newIndex, 0, program_obj.functions.splice(oldIndex, 1)[0]);
}

function runCodeAssessment() {
  toggleConsole(true);

  window.studentGrade = null;
  studentTemp = null;
  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }
  if (domConsole == null)
    domConsole = new DOMConsole("#ivprog-term");
  $("#ivprog-term").slideDown(500);
  const runner = new IVProgAssessment(strCode, testCases, domConsole);

  runner.runTest().then(grade => {
    if (!is_iassign) {
      parent.getEvaluationCallback(grade);
    } else {
      is_iassign = false;
    }
  }).catch(err => domConsole.err(err.message));

}

function runCode() {
  toggleConsole(true);

  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }
  if (domConsole == null)
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

    proc.interpretAST().then(_ => {
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

function toggleConsole(is_running) {

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

function waitToCloseConsole() {
  domConsole.info("Aperte qualquer tecla para fechar...");
  const p = new Promise((resolve, _) => {
    domConsole.requestInput(resolve, true);
  });
  p.then(_ => {
    domConsole.dispose();
    domConsole = null;
    $("#ivprog-term").hide();
  })
}

function toggleTextualCoding() {
  var code = CodeManagement.generate();
  $('.ivprog_visual_panel').css('display', 'none');
  $('.ivprog_textual_panel').css('display', 'block');
  $('.ivprog_textual_panel').removeClass('loading');
  $('.ivprog_textual_code').text(code);

  $('.visual_coding_button').removeClass('active');
  $('.textual_coding_button').addClass('active');
}

function toggleVisualCoding() {
  $('.ivprog_textual_panel').addClass('loading');
  $('.ivprog_textual_panel').css('display', 'none');
  $('.ivprog_visual_panel').css('display', 'block');

  $('.textual_coding_button').removeClass('active');
  $('.visual_coding_button').addClass('active');
}

function removeParameter(function_obj, parameter_obj, parameter_container) {
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

function renderParameter(function_obj, parameter_obj, function_container) {
  var ret = "";

  ret += '<div class="ui label function_name_parameter">';

  ret += '<div class="ui dropdown parameter_type">';

  if (parameter_obj.dimensions > 0) {
    ret += '<div class="text">' + LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(parameter_obj.type);
    if (parameter_obj.dimensions == 1) {
      ret += ' [ ] ';
    } else {
      ret += ' [ ] [ ] ';
    }
    ret += '</div>';
  } else {
    ret += '<div class="text">' + LocalizedStrings.getUI(parameter_obj.type) + '</div>';
  }

  ret += '<div class="menu">';


  for (var tm in Types) {
    if (tm == Types.VOID.toUpperCase()) {
      continue;
    }
    ret += '<div class="item ' + (parameter_obj.type == tm.toLowerCase() ? ' selected ' : '') + '" data-type="' + tm + '" >' + LocalizedStrings.getUI(tm.toLowerCase()) + '</div>';
  }

  for (var tm in Types) {
    if (tm == Types.VOID.toUpperCase()) {
      continue;
    }
    ret += '<div class="item">'
      + '<i class="dropdown icon"></i>'
      + LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(tm.toLowerCase())
      + '<div class="menu">'
      + '<div class="item" data-text="' + LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(tm.toLowerCase()) + ' [ ] " data-type="' + tm + '" data-dimensions="1">[ ]</div>'
      + '<div class="item" data-text="' + LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(tm.toLowerCase()) + ' [ ] [ ] " data-type="' + tm + '" data-dimensions="2">[ ] [ ] </div>'
      + '</div>'
      + '</div>';
  }

  ret += '</div></div>';

  ret += '<div class="parameter_div_edit"><span class="span_name_parameter label_enable_name_parameter">' + parameter_obj.name + '</span></div> ';

  ret += ' <i class="red icon times remove_parameter"></i></div>';

  ret = $(ret);

  function_container.find('.container_parameters_list').append(ret);

  ret.find('.remove_parameter').on('click', function (e) {
    removeParameter(function_obj, parameter_obj, ret);
  });

  ret.find('.ui.dropdown.parameter_type').dropdown({
    onChange: function (value, text, $selectedItem) {
      if ($selectedItem.data('dimensions')) {
        updateParameterType(parameter_obj, Types[$selectedItem.data('type')], $selectedItem.data('dimensions'));
      } else {
        updateParameterType(parameter_obj, Types[$selectedItem.data('type')]);
      }
    }
  });

  ret.find('.label_enable_name_parameter').on('click', function (e) {
    enableNameParameterUpdate(parameter_obj, ret);
  });

}

var opened_name_parameter = false;
var opened_input_parameter = null;
function enableNameParameterUpdate(parameter_obj, parent_node) {
  if (opened_name_parameter) {
    opened_input_parameter.focus();
    return;
  }
  opened_name_parameter = true;
  parent_node = $(parent_node);

  var input_field;

  parent_node.find('.span_name_parameter').text('');
  input_field = $("<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='" + parameter_obj.name + "' />");
  input_field.insertBefore(parent_node.find('.span_name_parameter'));

  input_field.on('input', function () {
    var inputWidth = input_field.textWidth() + 10;
    opened_input_parameter = input_field;
    input_field.focus();

    var tmpStr = input_field.val();
    input_field.val('');
    input_field.val(tmpStr);

    input_field.css({
      width: inputWidth
    })
  }).trigger('input');

  input_field.focusout(function () {
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

  input_field.on('keydown', function (e) {
    var code = e.keyCode || e.which;
    if (code == 13) {
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
    if (code == 27) {
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
function enableNameFunctionUpdate(function_obj, parent_node) {
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

  input_field = $("<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='" + function_obj.name + "' />");
  input_field.insertBefore(parent_node.find('.span_name_function'));

  input_field.on('input', function () {
    var inputWidth = input_field.textWidth() + 10;
    opened_input = input_field;
    input_field.focus();

    var tmpStr = input_field.val();
    input_field.val('');
    input_field.val(tmpStr);

    input_field.css({
      width: inputWidth
    })
  }).trigger('input');

  input_field.focusout(function () {
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

  input_field.on('keydown', function (e) {
    var code = e.keyCode || e.which;
    if (code == 13) {
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
    if (code == 27) {

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

/****************************************************
//DOUGLAS
*******************************************************/

export function generateMenuButton(function_obj) {
  if (function_obj.identifier) {
    var identifier = LocalizedStrings.getUI(system_functions[0].identifier);
    var menu_button = '<button class="fluid ui container segment labeled icon button list-group-item menu-item" draggable="true" data-function="' + identifier + '"><i class="list icon"></i> <span class="function_name">' + identifier + '</span> (<span class="function_params"></span>) : <span class="function_return_type">' + LocalizedStrings.getUI(function_obj.return_type) + '</span></button>';
  } else {
    var menu_button = '<button class="fluid ui container segment labeled icon button list-group-item menu-item" draggable="true" data-function="' + function_obj.name + '"><i class="list icon"></i> <span class="function_name">' + function_obj.name + '</span> (<span class="function_params"></span>) : <span class="function_return_type">' + LocalizedStrings.getUI(function_obj.return_type) + '</span></button>';
  }
  var params = "";
  menu_button = $(menu_button);

  for (var j = 0; j < function_obj.parameters_list.length; j++) {
    if (j > 0)
      params += ',';
    if (!function_obj.identifier) {
      params += LocalizedStrings.getUI(function_obj.parameters_list[j].type);
    } else {
      params += function_obj.parameters_list[j].type;
    }
  }
  menu_button
    .data('fun', function_obj)
    .on('dragstart', function (e) {
      program_obj.dataTransfer = { type: "function", content: function_obj };
      //e.originalEvent.dataTransfer.setData("text", JSON.stringify({type:"function",content:function_obj}));
      //evt.originalEvent.dataTransfer.setData("text",$(this).data('command'));
    })
    .find('.function_params').text(params)
    .find('.function_return_type').text(function_obj.type);

  menu_button
    .on('click', function (evt) {
      $(this).trigger('dragstart');
      if(window.ghostNode) {
        $(window.ghostNode).remove();
        $(document).off('mousemove');
      }
      window.ghostNode = $(this).clone();
      ghostNode.outerWidth($(this).outerWidth());
      ghostNode.draggable().appendTo('body');
      ghostNode.css('position', 'absolute');
      ghostNode.css('left', evt.pageX);
      ghostNode.css('top', evt.pageY);
      evt.type = 'drag';
      evt.target = ghostNode[0];
      ghostNode.trigger(evt);
      $(document).on('mousemove', function (evt) {
        ghostNode.css('left', evt.pageX);
        ghostNode.css('top', evt.pageY);
      });
    });
  return menu_button;
}

removeFunction = function (function_obj) {
  var index = program.functions.indexOf(function_obj);
  if (index > -1) {
    program.functions.splice(index, 1);
  }

  $('.functions_labels > [data-function=' + function_obj.name + ']').remove();
}

renderFunction = function (function_obj) {

  var appender = '<div class="ui secondary segment function_div list-group-item">';

  if (function_obj.function_comment) {
    //appender += renderComment(function_obj.function_comment, sequence, true, -1);
  }

  appender += '<span class="glyphicon glyphicon-move move_function" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>';

  appender += (function_obj.is_main ? '<div class="div_start_minimize_v"> </div>' : '<button class="ui icon button large remove_function_button"><i class="red icon times"></i></button>')
    + '<button class="ui icon button tiny minimize_function_button"><i class="icon window minimize"></i></button>';

  appender += '<div class="ui small icon buttons add_var_top_button"><div class="ui icon button add_var_button_function"><i class="icon superscript"></i></div>';

  appender += '<div class="ui icon button dropdown menu_commands" ><i class="icon code"></i> <div class="menu"> ';
  appender += '<a class="item" data-command="' + Models.COMMAND_TYPES.reader + '"><i class="download icon"></i> ' + LocalizedStrings.getUI('text_read_var') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.writer + '"><i class="upload icon"></i> ' + LocalizedStrings.getUI('text_write_var') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.comment + '"><i class="quote left icon"></i> ' + LocalizedStrings.getUI('text_comment') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.attribution + '"><i class="arrow left icon"></i> ' + LocalizedStrings.getUI('text_attribution') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.functioncall + '"><i class="hand point right icon"></i> ' + LocalizedStrings.getUI('text_functioncall') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.iftrue + '" ><i class="random icon"></i> ' + LocalizedStrings.getUI('text_iftrue') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.repeatNtimes + '"><i class="sync icon"></i> ' + LocalizedStrings.getUI('text_repeatNtimes') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.whiletrue + '"><i class="sync icon"></i> ' + LocalizedStrings.getUI('text_whiletrue') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.dowhiletrue + '"><i class="sync icon"></i> ' + LocalizedStrings.getUI('text_dowhiletrue') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.switch + '"><i class="list icon"></i> ' + LocalizedStrings.getUI('text_switch') + '</a>'
    + '<a class="item" data-command="' + Models.COMMAND_TYPES.return + '"><i class="reply icon"></i> ' + LocalizedStrings.getUI('text_btn_return') + '</a>'
    + '</div></div></div>';

  appender += '<div class="function_signature_div">' + LocalizedStrings.getUI("function") + ' ';

  if (function_obj.is_main) {
    appender += '<div class="function_name_div">  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ' + LocalizedStrings.getUI('void') + ' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <span class="span_name_function" >' + function_obj.name + '</span> </div> '
      + '( <div class="ui large labels parameters_list">';
  } else {
    appender += '<div class="ui function_return"></div>';

    appender += '<div class="function_name_div function_name_div_updated"><span class="span_name_function name_function_updated">' + function_obj.name + '</span> </div> '
      + ' <span class="parethesis_function"> ( </span> <i class="ui icon plus square outline add_parameter_button"></i> <div class="ui large labels parameters_list container_parameters_list">';

    $('.functions_labels').append(generateMenuButton(function_obj));
    console.log("aqui");

    //var menu_button = $('.functions_labels > [data-function=' + function_obj.name + ']');
    //var params = "";
    //menu_button

  }

  appender += '</div> ) </div>'
    + (function_obj.is_hidden ? ' <div class="function_area" style="display: none;"> ' : ' <div class="function_area"> ')

    + '<div class="ui top attached segment variables_list_div">'
    /*+ renderVariables(function_obj, sequence)*/
    + '</div>'
    + '<div class="ui bottom attached segment commands_list_div" id="function_drag_cmd_">';

  appender += '</div>';

  appender += '<div class="function_close_div"></div>'
    + '</div>'
    + '</div>';

  appender = $(appender);

  $('.all_functions').append(appender);

  appender.data('fun', function_obj);
  appender.find('.commands_list_div')
    .data('fun', function_obj)
    .attr('droppable', true)
    .on('dragenter', function (e) {
      e.preventDefault();
      console.log('dragenter');
      console.log(e.target)
      $(e.target).addClass('div-over')
      //e.stopPropagation();
    }).on('dragover', function (e) {
      e.preventDefault();
    })
    .on('dragleave', function (e) {
      e.preventDefault();
      //e.stopPropagation();
      console.log("dragleave")
      $(e.target).removeClass('div-over')
      console.log(e.target)
    })
    .on('drop', function (e, bundle) {
      e.preventDefault();
      if (bundle) {
        e.clientX = bundle.clientX;
        e.clientY = bundle.clientY;
      }
      //console.log('ondrop ' + e.originalEvent.dataTransfer.getData("text"));
      console.log(e)
      $(e.target).removeClass('div-over')
      //var data = JSON.parse(e.originalEvent.dataTransfer.getData("text"));
      var data = program_obj.dataTransfer;
      if (data.type == 'command')
        CommandsManagement.prepareManageCommand(function_obj, $(e.target).closest('.function_div'), e, data.content);
      else if (data.type == 'var')
        CommandsManagement.prepareManageCommand(function_obj, $(e.target).closest('.function_div'), e, "attribution", data.content);
      else {
        if (!data.content.parameters_list)
          data.content.parameters_list = [];
        CommandsManagement.prepareManageCommand(function_obj, $(e.target).closest('.function_div'), e, "functioncall", data.content);
      }
      //program_obj.dataTransfer;
    }).on('click', function (e) {
      if (window.ghostNode) {
        console.log("drop click");
        $(document).off('mousemove');
        $(window.ghostNode).remove();
        $(this).trigger('drop', [e]);
        $('.div-over').removeClass('div-over');
        delete window.ghostNode;
      }
    })
    .on('mouseover', function (evt) {
      if (window.ghostNode) {
        evt.type = 'dragenter';
        //$(this).trigger('dragenter');
        console.log('mouseover');
        $(this).trigger(evt);
      }
    })
    .on('mouseout', function (evt) {
      //$(this).trigger('dragleave');
      if (window.ghostNode) {
        evt.type = 'dragleave';
        console.log('mouseout');
        $(this).trigger(evt);
      }
    });

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
  console.log('kk')
  console.log($($('.function_div')[0]).data('fun'))
  console.log(appender.data('fun'))

}

initVisualUI = function () {
  // MUST USE CONST, LET, OR VAR !!!!!!
  const mainDiv = $('#visual-main-div');
  // fill mainDiv with functions and globals...
  // renderAlgorithm()...
  $('.add_function_button').on('click', () => {
    addFunctionHandler();
  });
  $('.add_global_button').on('click', () => {
    GlobalsManagement.addGlobal(program);
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

  var commands = [
    { type: Models.COMMAND_TYPES.break, icon: "stop", text: LocalizedStrings.getUI('text_break') },
    { type: Models.COMMAND_TYPES.reader, icon: "download", text: LocalizedStrings.getUI('text_read_var') },
    { type: Models.COMMAND_TYPES.writer, icon: "upload", text: LocalizedStrings.getUI('text_write_var') },
    { type: Models.COMMAND_TYPES.comment, icon: "quote left", text: LocalizedStrings.getUI('text_comment') },
    { type: Models.COMMAND_TYPES.attribution, icon: "arrow left", text: LocalizedStrings.getUI('text_attribution') },
    //{type: Models.COMMAND_TYPES.functioncall, icon: "hand point right", text: LocalizedStrings.getUI('text_functioncall')},
    { type: Models.COMMAND_TYPES.iftrue, icon: "random", text: LocalizedStrings.getUI('text_iftrue') },
    { type: Models.COMMAND_TYPES.repeatNtimes, icon: "sync", text: LocalizedStrings.getUI('text_repeatNtimes') },
    { type: Models.COMMAND_TYPES.whiletrue, icon: "sync", text: LocalizedStrings.getUI('text_whiletrue') },
    { type: Models.COMMAND_TYPES.dowhiletrue, icon: "sync", text: LocalizedStrings.getUI('text_dowhiletrue') },
    { type: Models.COMMAND_TYPES.switch, icon: "list", text: LocalizedStrings.getUI('text_switch') }
  ];

  for (var i = 0; i < commands.length; i++) {
    var command = '<button class="fluid ui container segment labeled icon button list-group-item menu-item" draggable="true"  data-command="' + commands[i].type + '"><i class="' + commands[i].icon + ' icon"></i> ' + commands[i].text + '</button>';
    command = $(command);
    command.on('dragstart', function (evt) {
      //evt.originalEvent.dataTransfer.setData("text",$(this).data('command'));
      //evt.originalEvent.dataTransfer.setData("text",JSON.stringify({type:"command",content:$(this).data('command')}));
      program_obj.dataTransfer = { type: "command", content: $(this).data('command') };
      console.log('dragstart')
      console.log(evt);

    });
    command.on('click', function (evt) {
      $(this).trigger('dragstart');
      if(window.ghostNode) {
        $(window.ghostNode).remove();
        $(document).off('mousemove');
      }
      window.ghostNode = $(this).clone();
      ghostNode.outerWidth($(this).outerWidth());
      ghostNode.draggable().appendTo('body');
      ghostNode.css('position', 'absolute');
      ghostNode.css('left', evt.pageX);
      ghostNode.css('top', evt.pageY);
      evt.type = 'drag';
      evt.target = ghostNode[0];
      ghostNode.trigger(evt);
      $(document).on('mousemove', function (evt) {
        ghostNode.css('left', evt.pageX);
        ghostNode.css('top', evt.pageY);
      });
    });
    $('.list-commands').prepend(command);
  }
  /*
    $(document).on('mousemove',function(evt) {
      if (typeof ghostNode !== 'undefined') {
        ghostNode.css('left', evt.pageX - 15);
        ghostNode.css('top', evt.pageY - 15);
      }
    });*/

  var library_labels = $('.library_labels');

  for (var i = 0; i < system_functions.length; i++) {
    var system_button = generateMenuButton(system_functions[i]);
    var category = system_functions[i].category;
    var cat_accord = null;
    if (library_labels.find('.' + category).length == 0) {
      cat_accord =
        '<div class="ui styled accordion ' + category + '"> \
          <div class="title"> \
            <i class="dropdown icon"></i> \
            ' + LocalizedStrings.getUI(category) + ' \
          </div> \
        <div class="content"><div>';
      library_labels.append(cat_accord);
    }
    cat_accord = $(library_labels.find('.' + category)[0]);
    cat_accord.find('.content').append(system_button);
  }

  $('.accordion').accordion();

}
/*
renderParameter = function(function_obj, parameter_obj, function_container) {
  var ret = "";

  ret += '<div class="ui label function_name_parameter"><span class="span_name_parameter label_enable_name_parameter">'+parameter_obj.name+'</span> <i class="icon small pencil alternate enable_edit_name_parameter label_enable_name_parameter"></i>';

  ret += '<div class="ui dropdown parameter_type">';

  if (parameter_obj.dimensions > 0) {
    ret += '<div class="text">'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(parameter_obj.type);
    ret += '</div>';
  } else {
    ret += '<div class="text">'+LocalizedStrings.getUI(parameter_obj.type)+'</div>';
  }

  ret += '<i class="dropdown icon"></i>'
    + '<div class="menu">';


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

  ret += ' <i class="red icon times remove_parameter"></i></div>';

  ret = $(ret);

  function_container.find('.container_parameters_list').append(ret);

  ret.find('.remove_parameter').on('click', function(e){
    removeParameter(function_obj, parameter_obj, ret);
  });

  ret.find('.ui.dropdown.parameter_type').dropdown({
    onChange: function(value, text, $selectedItem) {
      if ($($selectedItem).data('dimensions')) {
        updateParameterType(parameter_obj, Types[$($selectedItem).data('type')], $($selectedItem).data('dimensions'));
      } else {
        updateParameterType(parameter_obj, Types[$($selectedItem).data('type')]);
      }
    }
  });

  ret.find('.label_enable_name_parameter').on('click', function(e){
    enableNameParameterUpdate(parameter_obj, ret);
  });

}

enableNameFunctionUpdate = function(function_obj, parent_node) {
  if (opened_name_function) {
    $(opened_input).focus();
    return;
  }

  $(parent_node).find('.span_name_function').text('');
  $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+function_obj.name+"' />" ).insertBefore($(parent_node).find('.span_name_function'));

  $('.width-dynamic').on('input', function() {
    var inputWidth = $(this).textWidth()+10;
    opened_input = this;
    $(this).focus();

    var tmpStr = $(this).val();
    $(this).val('');
    $(this).val(tmpStr);

    $(this).css({
        width: inputWidth
    })
  }).trigger('input');

  $('.width-dynamic').focusout(function() {
    /// update array:
    if ($(this).val().trim()) {
      function_obj.name = $(this).val().trim();
    }
    $(this).remove();
    $(parent_node).find('.span_name_function').text(function_obj.name);

    /// update elements:
    opened_name_function = false;
    opened_input = false;
  });

  $('.width-dynamic').on('keydown', function(e) {
    var code = e.keyCode || e.which;
    if(code == 13) {
      $('.functions_labels > [data-function=' + function_obj.name + ']')
				.attr('data-function', $(this).val().trim())
				.html('<i class="list icon"></i> ' + $(this).val().trim());

      if ($(this).val().trim()) {
        function_obj.name = $(this).val().trim();
      }
      $(this).remove();

      $(parent_node).find('.span_name_function').text(function_obj.name);

      /// update elements:
      opened_name_function = false;
      opened_input = false;
    }
    if(code == 27) {

      $(this).remove();

      $(parent_node).find('.span_name_function').text(function_obj.name);

      /// update elements:
      opened_name_function = false;
      opened_input = false;
    }
  });

}

addParameter = function (function_obj, function_container) {
  if (function_obj.parameters_list == null) {
    function_obj.parameters_list = [];
  }
  var new_parameter = new Models.Variable(Types.INTEGER, LocalizedStrings.getUI("new_parameter") + "_" + counter_new_parameters);
  new_parameter.function_obj = function_obj;
  function_obj.parameters_list.push(new_parameter);
  counter_new_parameters ++;

  renderParameter(function_obj, new_parameter, function_container);

  //updateMenuButton(function_obj);
}

removeParameter = function (function_obj, parameter_obj, parameter_container) {
  var index = function_obj.parameters_list.indexOf(parameter_obj);
  if (index > -1) {
    function_obj.parameters_list.splice(index, 1);
  }
  $(parameter_container).remove();
}*/
/*
updateReturnType = function (function_obj, new_type, new_dimensions = 0) {
  function_obj.return_type = new_type;
  function_obj.return_dimensions = new_dimensions;

  var menu_button = $('.functions_labels > [data-function=' + function_obj.name + ']');
  menu_button.find('.function_return_type').text(LocalizedStrings.getUI(new_type));
}*/
/*
updateParameterType = function (parameter_obj, new_type, new_dimensions = 0) {
  parameter_obj.type = new_type;
  parameter_obj.dimensions = new_dimensions;

  if (new_dimensions > 0) {
    parameter_obj.rows = new_dimensions;
    parameter_obj.columns = 2;
  }

  //updateMenuButton(parameter_obj.function_obj);
}*/