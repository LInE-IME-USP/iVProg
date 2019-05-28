import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals';
import * as VariablesManagement from './variables';
import * as CommandsManagement from './commands';
import * as CodeManagement from './code_generator';
import * as VariableValueMenu from './commands/variable_value_menu';
import { DOMConsole } from './../io/domConsole';
import { IVProgProcessor } from './../processor/ivprogProcessor';
import WatchJS from 'melanke-watchjs';
import { SemanticAnalyser } from '../processor/semantic/semanticAnalyser';
import { IVProgAssessment } from '../assessment/ivprogAssessment';
import * as AlgorithmManagement from './algorithm';
import * as Utils from './utils';
import { registerUserEvent, ActionTypes } from "./../services/userLog";
import VersionInfo from './../../.ima_version.json';

var counter_new_functions = 0;
var counter_new_parameters = 0;
var ivprog_version = VersionInfo.version;

const globalChangeListeners = [];
const functionsChangeListeners = [];
let domConsole = null;
let _testCases = [];
window.studentGrade = null;
window.LocalizedStrings = LocalizedStrings;
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
window.system_functions.push(new Models.SystemFunction('$numElements', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.variable_and_function, null, null, null, true, 1)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.arrangement));
window.system_functions.push(new Models.SystemFunction('$matrixLines', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.variable_and_function, null, null, null, true, 2)],
  null, Models.SYSTEM_FUNCTIONS_CATEGORIES.arrangement));
window.system_functions.push(new Models.SystemFunction('$matrixColumns', Types.INTEGER, 0, [new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.variable_and_function, null, null, null, true, 2)],
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

console.log('       ___           ___                    ________          \n      /   /         /   /                  /   ____/  \n     /   /         /   /                  /   /        \n    /   /         /   /  ______    ___   /   /__         \n   /   /         /   /  /      \\  /  /  /   ___/      \n  /   /______   /   /  /   /\\   \\/  /  /   /      \n /          /  /   /  /   /  \\     /  /   /____     \n/__________/  /___/  /___/    \\___/  /________/       \n\n Laboratório de Informática na Educação\n http://line.ime.usp.br');

const mainFunction = new Models.Function(LocalizedStrings.getUI("start"), Types.VOID, 0, [], true, false);
mainFunction.function_comment = new Models.Comment(LocalizedStrings.getUI('text_comment_main'));
program.addFunction(mainFunction);

window.program_obj = program;

window.generator = CodeManagement.generate;
window.runCodeAssessment = runCodeAssessment;
window.renderAlgorithm = AlgorithmManagement.renderAlgorithm;
window.insertContext = false;
window.watchW = WatchJS;

WatchJS.watch(window.program_obj.globals, function(){
  if (window.insertContext) {
    setTimeout(function() {
      AlgorithmManagement.renderAlgorithm();
      globalChangeListeners.forEach(x => x());
    }, 300);
    window.insertContext = false;
  } else {
    AlgorithmManagement.renderAlgorithm();
    globalChangeListeners.forEach(x => x());
  }
}, 1);

WatchJS.watch(window.program_obj.functions, function(){
  if (window.insertContext) {
    setTimeout(function(){
      AlgorithmManagement.renderAlgorithm();
      functionsChangeListeners.forEach( x => x());
    }, 300);
    window.insertContext = false;
  } else {
    AlgorithmManagement.renderAlgorithm();
    functionsChangeListeners.forEach( x => x());
  }
}, 1);

function addFunctionHandler () {

	const new_function = new Models.Function(LocalizedStrings.getUI("new_function") + "_" + counter_new_functions, Types.VOID, 0, [], false, false, [], new Models.Comment(LocalizedStrings.getUI('text_comment_start')));
	program.addFunction(new_function);

	counter_new_functions ++;

  window.insertContext = true;
  registerUserEvent(new_function.name, ActionTypes.INSERT_FUNCTION);

  // var newe = renderFunction(new_function);
  renderFunction(new_function);
  /*newe.css('display', 'none');
  newe.fadeIn();*/
}

function addParameter (function_obj, function_container/*, is_from_click = false*/) {
  if (function_obj.parameters_list == null) {
    function_obj.parameters_list = [];
  }
  const new_parameter = new Models.Variable(Types.INTEGER, LocalizedStrings.getUI("new_parameter") + "_" + counter_new_parameters);
  function_obj.parameters_list.push(new_parameter);
  counter_new_parameters ++;
  registerUserEvent(function_obj.name, ActionTypes.INSERT_FUNCTION_PARAM, new_parameter.name, Types.INTEGER, 0);
  //var newe = renderParameter(function_obj, new_parameter, function_container);
  renderParameter(function_obj, new_parameter, function_container);
  // if (is_from_click) {
  //   newe.css('display', 'none');
  //   newe.fadeIn();
  // }
}

function updateReturnType (function_obj, new_type, new_dimensions = 0) {
  registerUserEvent(function_obj.name, ActionTypes.CHANGE_FUNCTION_RETURN, new_type, new_dimensions);
  function_obj.return_type = new_type;
  function_obj.return_dimensions = new_dimensions;
}

function removeFunction (function_obj) {
  var index = program.functions.indexOf(function_obj);
  if (index > -1) {
    registerUserEvent(function_obj.name, ActionTypes.REMOVE_FUNCTION);
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
      },
      selectOnKeydown: false
  });

  function_container.find( ".name_function_updated" ).on('click', function(e){
    enableNameFunctionUpdate(function_obj, function_container);
  });

  function_container.find( ".add_parameter_button" ).on('click', function(e){
    window.insertContext = true;
    addParameter(function_obj, function_container, true);
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
      registerUserEvent(function_obj.name, ActionTypes.INSERT_COMMAND, $(this).data('command'), '/', 0);
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
    function_container.fadeOut();
  });

  function_container.find('.minimize_function_button').on('click', function(e){
    minimizeFunction(function_obj);
    if (function_obj.is_hidden) {
      function_container.find(".add_var_button_function").toggle();
      function_container.find(".inline_add_command").toggle();
      function_container.find(".function_area").slideToggle();
    } else {
      function_container.find(".function_area").slideToggle(function(){
        function_container.find(".add_var_button_function").toggle();
        function_container.find(".inline_add_command").toggle();
      });
    }
    
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

var cont = 0;

export function renderFunction (function_obj) {
  
  var appender = '<div class="ui secondary segment function_div list-group-item function_cont_'+cont+'">';

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

  

  appender += '<div class="ui bottom attached segment commands_list_div commands_cont_'+cont+'">'
        + '<div class="ui rail" style="width: 35px; margin-left: -36px;"><div class="ui sticky sticky_cont_'+cont+'" style="top: 50px !important;">';


  appender += '<i class="icon plus circle purple"></i><i class="icon circle white back"></i><div class="ui icon button dropdown menu_commands orange" ><i class="icon code"></i> <div class="menu"> ';
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
        + '</div></div>';


  appender += '</div></div>'
        +'</div>';

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

  var function_index = program.functions.indexOf(function_obj);

  Sortable.create(appender.find(".variables_list_div")[0], {
    handle: '.ellipsis',
    animation: 100,
    ghostClass: 'ghost',
    group: 'local_vars_drag_' + function_index,
    onEnd: function (evt) {
       updateSequenceLocals(evt.oldIndex, evt.newIndex, function_obj);
    }
  });

  addSortableHandler(appender.find(".commands_list_div")[0], function_index);

  if (!function_obj.is_main) {
    Sortable.create(appender.find(".container_parameters_list")[0], {
      handle: '.ellipsis',
      animation: 100,
      ghostClass: 'ghost',
      group: 'parameters_drag_' + program.functions.indexOf(function_obj),
      onEnd: function (evt) {
         updateSequenceParameters(evt.oldIndex, evt.newIndex, function_obj);
      }
    });
  }

  if (function_obj.commands.length > 0) {
    var teste  = '.ui.sticky.sticky_cont_'+cont;
    $(teste).sticky({
      context: '.ui.bottom.attached.segment.commands_list_div.commands_cont_'+cont,
      scrollContext: '.ivprog_visual_panel',
      observeChanges: true,
      offset: 40,
      onStick: function (evt) {
        $(teste).css('top', '20px', 'important');
      }, 
      onBottom: function (evt) {
        $(teste).css('top', '20px', 'important');
      },
      onUnstick: function (evt) {
        $(teste).css('top', '20px', 'important');
      },
      onReposition: function (evt) {
        $(teste).css('top', '20px', 'important');
      },
      onScroll: function (evt) {
        $(teste).css('top', '20px', 'important');
        if (!isVisible($(teste), $(teste).parent())) {
          $(teste).removeClass('fixed');
        }
      },
      onTop: function (evt) {
        $(teste).css('top', '20px', 'important');
      }
    });
  }
  
  cont ++;

  return appender;
}

function isVisible (element, container) {

  var elementTop = $(element).offset().top,
      elementHeight = $(element).height(),
      containerTop = $(container).offset().top,
      containerHeight = $(container).height() - 30;

  return ((((elementTop - containerTop) + elementHeight) > 0)
         && ((elementTop - containerTop) < containerHeight));
}


window.evento_drag;

function updateProgramObjDrag () {
  const nodes = Array.prototype.slice.call( $('.all_functions').children() );
  let function_index;
  // var start_index;
  let function_obj;
  $(evento_drag.item).parentsUntil(".all_functions").each(function (index) {
    const ref = $(this);
    if (ref.hasClass('function_div')) {
      function_index = nodes.indexOf(this);
      start_index = index;
      function_obj = ref;
    }
  });

  console.log(function_index);

  const path_target = [];
  $(evento_drag.item).parentsUntil(".all_functions").each(function () {
    if ($(this).hasClass('command_container')) {
      path_target.push(this);
    }
  });
  if (path_target.length == 0) {
    //console.log('soltou na raiz, na posição: ' + evento_drag.newIndex + ' mas ainda não sei de onde saiu ');
  } else {
    //console.log('soltou dentro de algum bloco, sequência vem logo abaixo (de baixo pra cima): ');
    //console.log(path_target);
  }

  var index_each = [];
  var path_relative = [];
  for (var i = path_target.length - 1; i >= 0; i --) {
    console.log('da vez', $(path_target[i + 1]));
    if (i == (path_target.length - 1)) { // está na raiz
      var indice_na_raiz = function_obj.find('.command_container').index(path_target[i]);
      console.log('índice na raiz: ', indice_na_raiz);
    } else {
      if ($(path_target[i + 1]).hasClass('iftrue')) {
        if ($(path_target[i]).parent().hasClass('commands_if')) {
          path_relative.push('if');
          index_each.push($(path_target[i]).parent().find('.command_container').index(path_target[i]));
        } else {
          path_relative.push('else');
          index_each.push($(path_target[i]).parent().find('.command_container').index(path_target[i]));
        }
      } else if ($(path_target[i + 1]).hasClass('dowhiletrue')) {
        path_relative.push('dowhiletrue');
        index_each.push($(path_target[i + 1]).find('.command_container').index(path_target[i]));
      } else if ($(path_target[i + 1]).hasClass('repeatNtimes')) {
        path_relative.push('repeatNtimes');
        index_each.push($(path_target[i + 1]).find('.command_container').index(path_target[i]));
      } else if ($(path_target[i + 1]).hasClass('whiletrue')) {
        path_relative.push('whiletrue');
        index_each.push($(path_target[i + 1]).find('.command_container').index(path_target[i]));
      } else if ($(path_target[i + 1]).hasClass('switch')) {
        path_relative.push('switch');
        //index_each.push($(path_target[i + 1]).find('.command_container').index(path_target[i]));
      }
    }
  }
  // var index_in_block = -1;
  const is_in_else = $(evento_drag.item).parent().hasClass('commands_else');

  // index_in_block = $(evento_drag.item).parent().find('.command_container').index(evento_drag.item);

  const is_in_case_switch = $(evento_drag.item).parent().hasClass('case_commands_block');
  // var index_case_of_switch = -1;
  // if (is_in_case_switch) {
  //   index_case_of_switch = $(evento_drag.item).parent().parent().parent().find('.case_div').index($(evento_drag.item).parent().parent());
  // }

  /*console.log('path_relative:');
  console.log(path_relative);
  console.log('index_each:');
  console.log(index_each);
  console.log('index_in_block:');
  console.log(index_in_block);
  console.log('ele está em algum bloco de senão? ');
  console.log(is_in_else);
  console.log('ele está dentro de um case de switch?');
  console.log(is_in_case_switch);
  console.log('qual é o índice do case: ');
  console.log(index_case_of_switch);*/

  // encontrar o elemento na árvore:
  var command_start_point = window.program_obj.functions[function_index].commands[indice_na_raiz];
  var block_to_insert = command_start_point;
  for (var i = 0; i < index_each.length; i++) {
    if (path_relative[i] == "else") {
      block_to_insert = block_to_insert.commands_else[index_each[i]];
    } else if (path_relative[i] == "switch") {

    } else {
      block_to_insert = block_to_insert.commands_block[index_each[i]]
    }
  }

  //console.log('command_start_point', command_start_point);
  //console.log('block_to_insert', block_to_insert);

  // agora tem que alocar o comando na árvore, mas considerar as quatro situações:
  // (1) se está em um else ou (2) se está em switch ou (3) será um caso padrão ou (4) se será na raiz.
  
  if (path_target.length == 0) { // soltou na raiz:
    window.program_obj.functions[function_index].commands.splice(evento_drag.newIndex, 0, command_in_drag);
  } else if (is_in_else)  {
    if (block_to_insert.commands_else) {
      block_to_insert.commands_else.splice(evento_drag.newIndex, 0, command_in_drag);
    } else {
      block_to_insert.commands_else = [];
      block_to_insert.commands_else.push(command_in_drag);
    }
  } else if (is_in_case_switch) {

  } else {
    // verificar se tem alguma coisa no bloco:
    if (block_to_insert.commands_block) {
      block_to_insert.commands_block.splice(evento_drag.newIndex, 0, command_in_drag);
    } else {
      block_to_insert.commands_block = [];
      block_to_insert.commands_block.push(command_in_drag);
    }
  }

  window.draging = false;
  renderAlgorithm();
  

}

function prepareDragHandler (evt) {
  window.draging = true;

  var nodes = Array.prototype.slice.call( $('.all_functions').children() );
  var function_index;
  var function_obj;
  $(evt.item).parentsUntil(".all_functions").each(function (index) {
    if ($(this).hasClass('function_div')) {
      function_index = nodes.indexOf(this);
      function_obj = window.program_obj.functions[function_index];
    }
  });

  command_in_drag = $(evt.item).data("command");

  //console.log('$(evt.item).parent(): ');
  //console.log($(evt.item).parent());

  // descobrir qual das quatro situações:
  if ($(evt.item).parent().hasClass('commands_list_div')) { // está na raiz:
    if (function_obj.commands.indexOf(command_in_drag) > -1) {
      function_obj.commands.splice(function_obj.commands.indexOf(command_in_drag), 1);
    }
  } else if ($(evt.item).parent().hasClass('commands_else')) { // está no else:
    if ($(evt.item).parent().data('command').commands_else.indexOf(command_in_drag) > -1) {
      $(evt.item).parent().data('command').commands_else.splice($(evt.item).parent().data('command').commands_else.indexOf(command_in_drag), 1);
    }
  } else if ($(evt.item).parent().hasClass('case_commands_block')) { // está em um switch:

  } else { // caso padrão:
    if ($(evt.item).parent().data('command').commands_block.indexOf(command_in_drag) > -1) {
      $(evt.item).parent().data('command').commands_block.splice($(evt.item).parent().data('command').commands_block.indexOf(command_in_drag), 1);
    }
  }

}

var command_in_drag;

function addSortableHandler (element, id_function) {
  if(true) { // DISABLE COMMAND DRAG
    return;
  }
  var n_group = 'commands_drag_' + id_function;
  Sortable.create(element, {
    handle: '.command_drag',
    ghostClass: 'ghost',
    animation: 300,
    group: {name: n_group},
    onEnd: function (evt) {

      var nodes = Array.prototype.slice.call( $('.all_functions').children() );
      var function_index;
      var function_obj;
      $(evt.item).parentsUntil(".all_functions").each(function (index) {
        if ($(this).hasClass('function_div')) {
          function_index = nodes.indexOf(this);
          function_obj = window.program_obj.functions[function_index];
        }
      });

       registerUserEvent(function_obj.name, ActionTypes.MOVE_COMMAND, $(evt.item).data('command').type, '/', 'from: ' + evt.oldIndex + ' to: ' + evt.newIndex);
       //updateSequenceLocals(evt.oldIndex, evt.newIndex, function_obj);
       var itemEl = evt.item;  // dragged HTMLElement
       evt.to;    // target list
       evt.from;  // previous list
       evt.oldIndex;  // element's old index within old parent
       evt.newIndex;  // element's new index within new parent
       //console.log('::EVT::');
       //console.log(evt);

       window.evento_drag = evt;

       try {
        updateProgramObjDrag();
       } catch (e) {
        window.draging = false;
       }
    },
    onStart: function (evt) {
      //console.log("START::EVT::");
      //console.log(evt);
      //console.log("\n\ncommand_in_drag");
      try {
        prepareDragHandler(evt);
      } catch (e) {
        window.draging = false;
      }
    }
  });
  element = $(element);
  element.find(".iftrue").each(function( index ) {
    addSortableHandler($(this).find(".block_commands")[0], id_function);
    addSortableHandler($(this).find(".block_commands")[1], id_function);
  });

  element.find(".repeatNtimes").each(function( index ) {
    addSortableHandler($(this).find(".block_commands")[0], id_function);
  });

  element.find(".dowhiletrue").each(function( index ) {
    addSortableHandler($(this).find(".block_commands")[0], id_function);
  });

  element.find(".whiletrue").each(function( index ) {
    addSortableHandler($(this).find(".block_commands")[0], id_function);
  });

  element.find(".switch").each(function( index ) {

    $(this).find(".case_div").each(function( index ) {
      addSortableHandler($(this).find(".case_commands_block")[0], id_function);
    });

  });  
}

export function initVisualUI () {
  // MUST USE CONST, LET, OR VAR !!!!!!
  // const mainDiv = $('#visual-main-div');
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
  $('.expand_button').on('click', () => {
    full_screen();
  });
  $('.help_button').on('click', () => {
    window.open('https://www.usp.br/line/ivprog/', '_blank');
  });
  $('.main_title h2').prop('title', LocalizedStrings.getUI('text_ivprog_description'));

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
    content : LocalizedStrings.getUI("tooltip_help") + ' - ' + LocalizedStrings.getUI("text_ivprog_version") + ' ' + ivprog_version,
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

  var listGlobalsHandle = document.getElementById("listGlobalsHandle");
  Sortable.create(listGlobalsHandle, {
    handle: '.ellipsis',
    animation: 100,
    ghostClass: 'ghost',
    group: 'globals_divs_drag',
    onEnd: function (evt) {
       updateSequenceGlobals(evt.oldIndex, evt.newIndex);
    }
  });
}

export function setTestCases (testCases) {
  _testCases = testCases;
}

export function getTestCases () {
  // Deep clone of test cases to avoid unauthorized modification
  // TODO: It may be not possible to use this once custom test are fully implemented 
  return JSON.parse(JSON.stringify(_testCases));
}

var is_iassign = false;

function updateSequenceParameters (oldIndex, newIndex, function_obj) {
  function_obj.parameters_list.splice(newIndex, 0, function_obj.parameters_list.splice(oldIndex, 1)[0]);
}

function updateSequenceLocals (oldIndex, newIndex, function_obj) {
  function_obj.variables_list.splice(newIndex, 0, function_obj.variables_list.splice(oldIndex, 1)[0]);
}

function updateSequenceGlobals (oldIndex, newIndex) {
  program_obj.globals.splice(newIndex, 0, program_obj.globals.splice(oldIndex, 1)[0]);
}

function updateSequenceFunction (oldIndex, newIndex) {
  program_obj.functions.splice(newIndex, 0, program_obj.functions.splice(oldIndex, 1)[0]);
}

function runCodeAssessment () {
  
  window.studentGrade = null;
  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }

  toggleConsole(true);

  if(domConsole == null)
    domConsole = new DOMConsole("#ivprog-term");
  $("#ivprog-term").slideDown(500);
  const runner = new IVProgAssessment(strCode, _testCases, domConsole);

  runner.runTest().then(grade => {
    if (!is_iassign) {
      parent.getEvaluationCallback(grade);
    } else {
      is_iassign = false;
    }
  }).catch( err => console.log(err));
  
}

function runCode () {
  
  const strCode = CodeManagement.generate();
  if (strCode == null) {
    return;
  }
  
  toggleConsole(true);

  if(domConsole == null)
    domConsole = new DOMConsole("#ivprog-term");
  $("#ivprog-term").slideDown(500);
  try {
    const data = SemanticAnalyser.analyseFromSource(strCode);
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

// function waitToCloseConsole () {
//   domConsole.info("Aperte qualquer tecla para fechar...");
//   const p = new Promise((resolve, _) => {
//     domConsole.requestInput(resolve, true);
//   });
//   p.then( _ => {
//     domConsole.dispose();
//     domConsole = null;
//     $("#ivprog-term").hide();
//   })
// }

function toggleTextualCoding () {
  var code = CodeManagement.generate();

  if (code == null) {
    return;
  }

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
  registerUserEvent(parameter_obj.name, ActionTypes.REMOVE_FUNCTION_PARAM, function_obj.name);
  var index = function_obj.parameters_list.indexOf(parameter_obj);
  if (index > -1) {
    window.insertContext = true;
    function_obj.parameters_list.splice(index, 1);
  }
  $(parameter_container).fadeOut();
}

function updateParameterType (parameter_obj, new_type, function_name, new_dimensions = 0) {
  registerUserEvent(parameter_obj.name, ActionTypes.CHANGE_PARAM_TYPE, function_name, new_type, new_dimensions);
  parameter_obj.type = new_type;
  parameter_obj.dimensions = new_dimensions;

  if (new_dimensions > 0) {
    parameter_obj.rows = new_dimensions;
    parameter_obj.columns = 2;
  }

}

function renderParameter (function_obj, parameter_obj, function_container) {
  let ret = "";

  ret += '<div class="ui label function_name_parameter pink"><i class="ui icon ellipsis vertical inverted"></i>';

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

  
  for (const tm in Types) {
      if (tm == Types.VOID.toUpperCase()) {
        continue;
      }
      ret += '<div class="item ' + (parameter_obj.type == tm.toLowerCase() ? ' selected ' : '') + '" data-type="'+tm+'" >'+LocalizedStrings.getUI(tm.toLowerCase())+'</div>';
  }

  for (const tm in Types) {
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

  ret += ' <i class="yellow inverted icon times remove_parameter"></i></div>';

  ret = $(ret);
  
  function_container.find('.container_parameters_list').append(ret);

  ret.find('.remove_parameter').on('click', function(e){
    removeParameter(function_obj, parameter_obj, ret);
  });
  
  ret.find('.ui.dropdown.parameter_type').dropdown({
    onChange: function(_, __, $selectedItem) {
      if ($selectedItem.data('dimensions')) {
        updateParameterType(parameter_obj, Types[$selectedItem.data('type')], function_obj.name, $selectedItem.data('dimensions'));
      } else {
        updateParameterType(parameter_obj, Types[$selectedItem.data('type')], function_obj.name);
      }
    },
    selectOnKeydown: false
  });

  ret.find('.label_enable_name_parameter').on('click', function(e){
    registerUserEvent(function_obj.name, ActionTypes.ENTER_CHANGE_PARAM_NAME, parameter_obj.name);
    enableNameParameterUpdate(parameter_obj, ret, function_obj);
  });

  return ret;
}

function updateParameterName (parameter_var, new_name, parameter_obj_dom, function_obj) {
  
  if (parameter_var.name == new_name) {
    return;
  }

  if (isValidIdentifier(new_name)) {
    if (variableNameAlreadyExists(new_name, function_obj)) {
      Utils.renderErrorMessage(parameter_obj_dom.find('.parameter_div_edit'), LocalizedStrings.getUI('inform_valid_variable_duplicated'));
    } else {
      registerUserEvent(parameter_var.name, ActionTypes.RENAME_FUNCTION_PARAM, function_obj.name, new_name);
      parameter_var.name = new_name;
    }
  } else {
    Utils.renderErrorMessage(parameter_obj_dom.find('.parameter_div_edit'), LocalizedStrings.getUI('inform_valid_name'));
  }
}

function variableNameAlreadyExists (name_var, function_obj) {

  if (function_obj.parameters_list) {
    for (var i = 0; i < function_obj.parameters_list.length; i++) {
      if (function_obj.parameters_list[i].name == name_var) {
        return true;
      }
    }
  }

  if (function_obj.variables_list) {
    for (var i = 0; i < function_obj.variables_list.length; i++) {
      if (function_obj.variables_list[i].name == name_var) {
        return true;
      }
    }
  }

  return false;
}

function updateFunctionName (function_var, new_name, function_obj_dom) {
  
  if (function_var.name == new_name) {
    return;
  }
  
  if (isValidIdentifier(new_name)) {
    if (functionNameAlreadyExists(new_name)) {
      Utils.renderErrorMessage(function_obj_dom.find('.function_name_div'), LocalizedStrings.getUI('inform_valid_name_duplicated'));
    } else {
      registerUserEvent(function_var.name, ActionTypes.RENAME_FUNCTION, new_name);
      function_var.name = new_name;
    }
  } else {
    Utils.renderErrorMessage(function_obj_dom.find('.function_name_div'), LocalizedStrings.getUI('inform_valid_name'));
  }
}

function functionNameAlreadyExists (function_name) {
  for (var i = 0; i < window.program_obj.functions.length; i++) {
    if (window.program_obj.functions[i].name == function_name) {
      return true;
    }
  }
  return false;
}

function isValidIdentifier (identifier_str) {
  return /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(identifier_str);
}

var opened_name_parameter = false;
var opened_input_parameter = null;
function enableNameParameterUpdate (parameter_obj, parent_node, function_obj) {
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
      updateParameterName(parameter_obj, input_field.val().trim(), parent_node, function_obj);
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
        updateParameterName(parameter_obj, input_field.val().trim(), parent_node, function_obj);
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
      updateFunctionName(function_obj, input_field.val().trim(), parent_node);
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
        updateFunctionName(function_obj, input_field.val().trim(), parent_node);
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

export function addFunctionChangeListener (callback) {
  functionsChangeListeners.push(callback);
  return functionsChangeListeners.length - 1;
}

export function addGlobalChangeListener (callback) {
  globalChangeListeners.push(callback);
  return globalChangeListeners.length - 1;
}

export function removeGlobalListener (index) {
  globalChangeListeners.splice(index, 1);
}

export function removeFunctionListener (index) {
  functionsChangeListeners.splice(index);
}