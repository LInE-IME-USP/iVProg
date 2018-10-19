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


var counter_new_functions = 0;
var counter_new_parameters = 0;

let domConsole = null;
let studentGrade = null;
const program = new Models.Program();
/*const variable1 = new Models.Variable(Types.INTEGER, "a", 1);
const parameter1 = new Models.Variable(Types.INTEGER, "par_1", 1);
const command1 = new Models.Comment(new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.only_value, "Testing rendering commands"));

const sumFunction = new Models.Function("soma", Types.INTEGER, 0, [parameter1], false, false, [], null, [command1]);


program.addFunction(sumFunction);
*/

const mainFunction = new Models.Function(LocalizedStrings.getUI("start"), Types.VOID, 0, [], true, false);
mainFunction.function_comment = new Models.Comment(LocalizedStrings.getUI('text_comment_main'));
program.addFunction(mainFunction);

window.program_obj = program;

WatchJS.watch(program.globals, function(){
      console.log("as globais foram alteradas!");
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
        $selectedItem = $($selectedItem);
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
    VariablesManagement.addVariable(function_obj, function_container);
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
      ret += '</div>';
    } else {
      ret += '<div class="text">'+LocalizedStrings.getUI(function_obj.return_type)+'</div>';
    }

    ret += '<i class="dropdown icon"></i>'
      + '<div class="menu">';


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


function renderFunction (function_obj) {

  var appender = '<div class="ui secondary segment function_div list-group-item">';

  if (function_obj.function_comment) {
    //appender += renderComment(function_obj.function_comment, sequence, true, -1);
  }
    
  appender += '<span class="glyphicon glyphicon-move move_function" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>';

  appender += (function_obj.is_main ? '<div class="div_start_minimize_v"> </div>' : '<button class="ui icon button large remove_function_button"><i class="red icon times"></i></button>')
    + '<button class="ui icon button tiny minimize_function_button"><i class="icon window minimize"></i></button>';

  appender += '<div class="ui small icon buttons add_var_top_button"><div class="ui icon button add_var_button_function"><i class="icon superscript"></i></div>';
  
  appender += '<div class="ui icon button dropdown menu_commands" ><i class="icon code"></i> <div class="menu"> ';
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
        + '</div></div></div>';

  appender += '<div class="function_signature_div">'+LocalizedStrings.getUI("function")+' ';

  if (function_obj.is_main) {
      appender += '<div class="function_name_div">  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ' + LocalizedStrings.getUI('void') + ' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <span class="span_name_function" >'+function_obj.name+'</span> </div> '
        + '( <div class="ui large labels parameters_list">';
  } else {
      appender += '<div class="ui function_return"></div>';

      appender += '<div class="function_name_div"><span class="span_name_function name_function_updated">'+function_obj.name+'</span> <i class="icon small pencil alternate enable_edit_name_function name_function_updated"></i></div> ' 
        + '( <i class="ui icon plus square outline add_parameter_button"></i> <div class="ui large labels parameters_list container_parameters_list">';
  }
    
  appender += '</div> ) {</div>'
    + (function_obj.is_hidden ? ' <div class="function_area" style="display: none;"> ' : ' <div class="function_area"> ')

    + '<div class="ui top attached segment variables_list_div">'
    /*+ renderVariables(function_obj, sequence)*/
    + '</div>'
    + '<div class="ui bottom attached segment commands_list_div" id="function_drag_cmd_">';


  if (function_obj.commands) {
    for (var l = 0; l < function_obj.commands.length; l++) {
      //appender += renderElementCommandGeneric(programa.funcoes[sequence].comandos[l], sequence, l, -1, l);
      
    }
  }

  appender += '</div>';

  appender += '<div class="function_close_div">}</div>'
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
    toggleTextualCoding();
  });
}

$( document ).ready(function() {

  for (var i = 0; i < program.functions.length; i++) {
    renderFunction(program.functions[i]);
  }

});


function runCodeAssessment () {
  studentGrade = null;
  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }
  domConsole = new DOMConsole("#ivprog-term", testCases);
  $("#ivprog-term").slideDown(500);
  const testCases = [];
  const runner = new IVProgAssessment(strCode, testCases, domConsole);
  runner.runTest().then(grade => studentGrade = grade);
  while(studentGrade == null) {
    continue;
  }
  waitToCloseConsole()
}

function runCode () {
  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }
  domConsole = new DOMConsole("#ivprog-term");
  $("#ivprog-term").slideDown(500);
  try {
    const parser = IVProgParser.createParser(strCode);
    const analyser = new SemanticAnalyser(parser.parseTree());
    const data = analyser.analyseTree();
    const proc = new IVProgProcessor(data);
    proc.registerInput(domConsole);
    proc.registerOutput(domConsole);
    
    proc.interpretAST().then( _ => {
      domConsole.info("Programa executado com sucesso!");
      waitToCloseConsole();
    }).catch(err => {
      domConsole.err(err.message);
      waitToCloseConsole();
    }) 
  } catch (error) {
    domConsole.err(error.message);
    waitToCloseConsole();
    console.log(error);
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
}

function toggleVisualCoding () {
  $('.ivprog_textual_panel').addClass('loading');
  $('.ivprog_textual_panel').css('display', 'none');
  $('.ivprog_visual_panel').css('display', 'block');
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

var opened_name_parameter = false;
var opened_input_parameter = null;
function enableNameParameterUpdate (parameter_obj, parent_node) {
  if (opened_name_parameter) {
    $(opened_input_parameter).focus();
    return;
  }
  opened_name_parameter = true;

  $(parent_node).find('.span_name_parameter').text('');
  $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+parameter_obj.name+"' />" ).insertBefore($(parent_node).find('.span_name_parameter'));

  $('.width-dynamic').on('input', function() {
    var inputWidth = $(this).textWidth()+10;
    opened_input_parameter = this;
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
      parameter_obj.name = $(this).val().trim();
      $(parent_node).find('.span_name_parameter').text(parameter_obj.name);
    }
    $(this).remove();

    /// update elements:
    opened_name_parameter = false;
    opened_input_parameter = false;
  });

  $('.width-dynamic').on('keydown', function(e) {
    var code = e.keyCode || e.which;
    if(code == 13) {
      if ($(this).val().trim()) {
        parameter_obj.name = $(this).val().trim();
        $(parent_node).find('.span_name_parameter').text(parameter_obj.name);
      }
      $(this).remove();

      /// update elements:
      opened_name_parameter = false;
      opened_input_parameter = false;

    }
    if(code == 27) {
      $(parent_node).find('.span_name_parameter').text(parameter_obj.name);

      $(this).remove();

      /// update elements:
      opened_name_parameter = false;
      opened_input_parameter = false;
    }
  });

}

var opened_name_function = false;
var opened_input = null;
function enableNameFunctionUpdate(function_obj, parent_node) {
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