// Função para ler parâmetros informados pelo iTarefa via URL
// Apesar de não ser obrigatório, será muito útil para capturar os parâmetros
function getParameterByName (name, defaultReturn = null) {
    var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search);
    return match ? decodeURIComponent(match[1].replace(/\+/g, ' ')) : defaultReturn;
}

// Criando um object com os parâmetros informados pelo iTarefa
// Observe que para cada parâmetro, é realizada a chamada do método getParameterByName, implementado acima
var iLMparameters = {
    iLM_PARAM_ServerToGetAnswerURL: getParameterByName("iLM_PARAM_ServerToGetAnswerURL"),
    iLM_PARAM_SendAnswer: getParameterByName("iLM_PARAM_SendAnswer"),
    iLM_PARAM_AssignmentURL: getParameterByName("iLM_PARAM_AssignmentURL"),
    iLM_PARAM_Assignment: getParameterByName("iLM_PARAM_Assignment"),
    lang: getParameterByName("lang", "pt")
};

// Set the lang parameter to the localStorage for easy access
// and no dependency to the global scope, avoind future 'strict mode' problems
//localStorage.setItem('ivprog.lang', iLMparameters.lang);

// Função chamada pelo iTarefa quando o professor finaliza a criação da atividade
// ou quando o aluno finaliza a resolução do exercício
// O retorno é um JSON com os dados do exercício ou da resolução
// Esse retorno será armazenado no banco de dados do Moodle, pelo iTarefa
function getAnswer () {
    // Se o parâmetro "iLM_PARAM_SendAnswer" for false,
    // então trata-se de resolução de atividade
    if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
        // Montar o retorno com a resposta do aluno
        var contentToSend = previousContent.split("\n::algorithm::")[0];
        contentToSend += '\n::algorithm::\n';
        contentToSend += JSON.stringify(window.program_obj, function(key, value) {
            if (key == 'dom_object') {
                return;
            }
            return value; 
        });

        contentToSend += '\n::logs::';
        contentToSend += getTrackingLogs();

        return contentToSend;

    } else {
        // Montar o retorno com a criação da atividade do professor
        var ret = ' { ' + prepareTestCases() 
            + ',\n"settings_data_types": \n' + JSON.stringify($('form[name="settings_data_types"]').serializeArray()) 
            + ',\n"settings_commands": \n' + JSON.stringify($('form[name="settings_commands"]').serializeArray()) 
            + ',\n"settings_functions": \n' + JSON.stringify($('form[name="settings_functions"]').serializeArray()) 
            + ' } ';

        if ($("input[name='include_algo']").is(':checked')) {
            ret += '\n::algorithm::\n';
            ret += JSON.stringify(window.program_obj, function(key, value) {
                
                if (key == 'dom_object') {
                    return;
                }
                return value; 
            });
        }

        return ret;

    }
}

function prepareTestCases () {
    var ret = ' \n "testcases" : [ '
    var test_cases_array = $('form[name="test_cases"]').serializeArray();
    for (var i = 0; i < test_cases_array.length; i = i + 2) {
        ret += '\n{ ';
        ret += '\n "input": [';
        var inps = test_cases_array[i].value.match(/[^\r\n]+/g);
        if (inps) {
            for (var j = 0; j < inps.length; j++) {
                ret += '"' + inps[j] + '"';
                if ((j + 1) < inps.length) {
                    ret += ', ';
                }
            }
        }
        ret += '], \n "output": [';
        var outs = test_cases_array[i+1].value.match(/[^\r\n]+/g);
        if (outs) {
            for (var j = 0; j < outs.length; j++) {
                ret += '"' + outs[j] + '"';
                if ((j + 1) < outs.length) {
                    ret += ', ';
                }
            }
        }
        ret += ']';
        ret += '\n}'
        if ((i + 2) < test_cases_array.length) {
            ret += ',';
        }
    }
    ret += '\n] ';
    return ret;
}

// Função chamada pelo iTarefa para receber a nota do aluno na atividade
// O retorno é um valor entre 0.0 e 1.0
function getEvaluation () {
    if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
        // A chamada do método abaixo é obrigatória!
        // Observe que a chamada parte do iLM para o iTarefa
        //parent.getEvaluationCallback(window.studentGrade);

        runCodeAssessment();
    }
}


var testCases = null;
var settingsDataTypes = null;
var settingsCommands = null;
var settingsFunctions = null;
var algorithm_in_ilm = null;
var previousContent = null;

// Função para que o iMA leia os dados da atividade fornecidos pelo iTarefa
function getiLMContent () {

    // O parâmetro "iLM_PARAM_Assignment" fornece o URL do endereço que deve ser
    // requisitado via AJAX para a captura dos dados da atividade
    $.get(iLMparameters.iLM_PARAM_Assignment, function (data) {
        // Aluno está trabalhando em alguma atividade:
        if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
            previousContent = data;
            prepareActivityToStudent(data);
        } else { // Professor está editando uma atividade:
            previousContent = data;
            prepareActivityToEdit(data);
        }

        window.block_render = false;
        renderAlgorithm();
    });
}

function prepareActivityToEdit (ilm_cont) {
    var content = JSON.parse(ilm_cont.split('\n::algorithm::')[0]);
    testCases = content.testcases;
    settingsDataTypes = content.settings_data_types;
    settingsCommands = content.settings_commands;
    settingsFunctions = content.settings_functions;

    for (var i = 0; i < testCases.length; i++) {
        addTestCase(testCases[i]);
    }

    if (ilm_cont.split('\n::algorithm::')[1]) {
        algorithm_in_ilm = ilm_cont.split('\n::algorithm::')[1].split('\n::logs::')[0];
        $("input[name='include_algo']").prop('checked', true);
        includePreviousAlgorithm();
        renderAlgorithm();
    }
}

function includePreviousAlgorithm () {
    window.program_obj.functions = JSON.parse(algorithm_in_ilm).functions;
    window.program_obj.globals = JSON.parse(algorithm_in_ilm).globals;

    window.watchW.watch(window.program_obj.globals, function(){
      if (window.insertContext) {
        setTimeout(function(){ renderAlgorithm(); }, 300);
        window.insertContext = false;
      } else {
        renderAlgorithm();
      }
    }, 1);

    for (var i = 0; i < window.program_obj.functions.length; i ++) {
        window.watchW.watch(window.program_obj.functions[i].parameters_list, function(){
          if (window.insertContext) {
            setTimeout(function(){ renderAlgorithm(); }, 300);
            window.insertContext = false;
          } else {
            renderAlgorithm();
          }
        }, 1);

        window.watchW.watch(window.program_obj.functions[i].variables_list, function(){
          if (window.insertContext) {
            setTimeout(function(){ renderAlgorithm(); }, 300);
            window.insertContext = false;
          } else {
            renderAlgorithm();
          }
        }, 1);

        if (window.program_obj.functions[i].is_main) {
            window.program_obj.functions[i].name = LocalizedStrings.getUI("start");
        }
    }
    window.watchW.watch(window.program_obj.functions, function(){
      if (window.insertContext) {
        setTimeout(function(){ renderAlgorithm(); }, 300);
        window.insertContext = false;
      } else {
        renderAlgorithm();
      }
    }, 1);
}

function prepareActivityToStudent (ilm_cont) {
    var content = JSON.parse(ilm_cont.split('\n::algorithm::')[0]);
    testCases = content.testcases;
    settingsDataTypes = content.settings_data_types;
    settingsCommands = content.settings_commands;
    settingsFunctions = content.settings_functions;

    if (ilm_cont.split('\n::algorithm::')[1]) {
        algorithm_in_ilm = ilm_cont.split('\n::algorithm::')[1].split('\n::logs::')[0];
        includePreviousAlgorithm();
    }
    $('.assessment_button').removeClass('disabled');
    renderAlgorithm();
}

// Função para organizar se para criação, visualização ou resolução de atividade
function prepareEnvironment () {
    if ((iLMparameters.iLM_PARAM_AssignmentURL == "true") && (iLMparameters.iLM_PARAM_SendAnswer == "true")) {
        prepareActivityCreation();
    }
}

$(document).ready(function() {

    // Se iLM_PARAM_SendAnswer for false, então trata-se de resolução de atividade,
    // portanto, a "DIV" de resolução é liberada
    if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
        //$('.resolucao').css("display","block");
        getiLMContent();


        $( document ).ready(function() {
            $('.div_to_body').mousemove(function(e) {
                trackingMatrix.push(adCoords(e, 0));
            });

            $('.div_to_body').click(function(e) {
                trackingMatrix.push(adCoords(e, 1));                    
            });

        });

    } else {
        // Caso não esteja em modo de resolução de atividade, a visualização no momento
        // é para a elaboração de atividade:
        //$('.elaboracao').css("display","block");

        // Se possuir o parâmetro iLMparameters.iLM_PARAM_Assignment, o professor
        // está editando uma atividade:
        if (iLMparameters.iLM_PARAM_Assignment) {
            getiLMContent();
        }
    }

    if (!testCases) {
        $('.assessment_button').addClass('disabled');
    }

});

// Função para preparar a interface para o professor criar atividade:
function prepareActivityCreation () {

    $('.add_accordion').addClass('accordion');

    $('.default_visual_title').toggle();
    $('.default_visual_title').append('<span>'+LocalizedStrings.getUI('text_teacher_algorithm')+'</span>');
    $('.height_100').removeClass('height_100');
    $('.main_title').remove();
    $('.ui.accordion').addClass('styled');
    
    $('<div class="content_margin"></div>').insertBefore($('.add_accordion').find('.content').find('.div_to_body'));

    $('<div class="ui checkbox"><input type="checkbox" name="include_algo" class="include_algo" tabindex="0" class="hidden"><label>'+LocalizedStrings.getUI('text_teacher_algorithm_include')+'</label></div>').insertAfter('.content_margin');
    
    var cases_test_div = $('<div class="ui accordion styled"><div class="active title"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('text_teacher_test_case')+'</div><div class="active content"></div></div>');

    cases_test_div.insertBefore('.accordion');

    var config_div = $('<div class="ui accordion styled"><div class="title"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('text_teacher_config')+'</div><div class="content"></div></div>');

    config_div.insertAfter(cases_test_div);

    $('.ui.accordion').accordion();

    $('.ui.checkbox').checkbox();

    prepareTableSettings(config_div.find('.content'));

    prepareTableTestCases(cases_test_div.find('.content'));
}

function prepareTableTestCases (div_el) {

    var table_el = '<form name="test_cases"><table class="ui blue table"><thead><tr><th width="30px">#</th><th>'+LocalizedStrings.getUI('text_teacher_test_case_input')+'</th><th>'+LocalizedStrings.getUI('text_teacher_test_case_output')+'</th><th width="80px">'+LocalizedStrings.getUI('text_teacher_test_case_actions')+'</th></tr></thead>'
            + '<tbody class="content_cases"></tbody></table></form>';

    div_el.append(table_el);

    div_el.append('<button class="ui teal labeled icon button button_add_case"><i class="plus icon"></i>'+LocalizedStrings.getUI('text_teacher_test_case_add')+'</button>');

    $('.button_add_case').on('click', function(e) {
        addTestCase();
    });
}

var hist = false;

function addTestCase (test_case = null) {
    var new_row = null;
    if (test_case) {
        var text_row = '';

        text_row += '<tr><td class="counter"></td><td class="expandingArea"><textarea rows="'+test_case.input.length+'" name="input" class="text_area_input">';

        for (var i = 0; i < test_case.input.length; i ++) {
            text_row += test_case.input[i];
            if ((i + 1) < test_case.input.length) {
                text_row += '\n';
            }
        }
        
        text_row += '</textarea></td><td class="expandingArea"><textarea rows="'+test_case.output.length+'" name="output" class="text_area_output">';

        for (var i = 0; i < test_case.output.length; i ++) {
            text_row += test_case.output[i];
            if ((i + 1) < test_case.output.length) {
                text_row += '\n';
            }
        }

        text_row += '</textarea></td><td class="btn_actions"><div class="ui button_remove_case"><i class="red icon times large"></i></div></td></tr>';

        new_row = $(text_row);
    } else {
        new_row = $('<tr><td class="counter"></td><td class="expandingArea"><textarea rows="1" name="input" class="text_area_input"></textarea></td><td class="expandingArea"><textarea rows="1" name="output" class="text_area_output"></textarea></td><td class="btn_actions"><div class="ui button_remove_case"><i class="red icon times large"></i></div></td></tr>');
    }
    $('.content_cases').append(new_row);

    new_row.find('.button_remove_case').click(function(e) {
        new_row.remove();
        updateTestCaseCounter();
    });

    new_row.find('textarea').on('input', function(e) {
        var lines = $(this).val().split('\n').length;
        $(this).attr('rows', lines);
    });
    
    updateTestCaseCounter();

     $('.text_area_output').keydown(function(e) {
        var code = e.keyCode || e.which;
        if (code == 9 && $(this).closest("tr").is(":last-child")) {
            hist = true;
            addTestCase();
        }
     });
     if (test_case == null) {
        if (!hist) {
            $( ".content_cases tr:last" ).find('.text_area_input').focus();
         } else {
            hist = false;
         }
     }
}

function updateTestCaseCounter () {
    var i = 1;
    $( ".content_cases" ).find('tr').each(function() {
      $( this ).find('.counter').text(i);
      i ++;
    });
}

function prepareTableSettings (div_el) {
    div_el.append('<h4 class="ui header">'+LocalizedStrings.getUI('text_teacher_data_types')+'</h4>');
    div_el.append('<form name="settings_data_types"><div class="ui stackable five column grid">'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="integer_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('integer')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="real_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('real')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="text_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="boolean_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('boolean')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="void_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('void')+'</label></div></div>'
        +'</div></form>');


    div_el.append('<h4 class="ui header">'+LocalizedStrings.getUI('text_teacher_commands')+'</h4>');
    div_el.append('<form name="settings_commands"><div class="ui stackable three column grid">'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_read" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_read_var')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_write" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_write_var')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_comment" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_comment')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_attribution" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_attribution')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_functioncall" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_functioncall')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_iftrue" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_iftrue')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_repeatNtimes" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_repeatNtimes')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_while" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_whiletrue')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_dowhile" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_dowhiletrue')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="commands_switch" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_switch')+'</label></div></div>'
        +'</div></form>');

    div_el.append('<h4 class="ui header">'+LocalizedStrings.getUI('text_teacher_functions')+'</h4>');
    div_el.append('<form name="settings_functions"><div class="ui stackable one column grid">'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="functions_creation" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_teacher_create_functions')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="functions_move" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_teacher_create_movement_functions')+'</label></div></div>'
        +'</div></form>');

    $('.ui.checkbox').checkbox();


}

function getTrackingLogs () {
    var ret = "";
    for (var i = 0; i < trackingMatrix.length; i++) {
        ret += "\n" + trackingMatrix[i][0] + "," + trackingMatrix[i][1] + "," + trackingMatrix[i][2];
        if (trackingMatrix[i][2] === 1) {
            ret += ',"' + trackingMatrix[i][3] + '"';
        }
    }
    return ret;
}

// Tracking mouse movements
var trackingMatrix = [];

function adCoords(e, code){
    var x = e.pageX; 
    var y = e.pageY;
    if (code === 1) {
        return [x, y, code, e.target.classList['value']];
    } else {
        return [x, y, code];
    }
}

$( document ).ready(function() {

    if (inIframe()) {
        orderIcons();
        orderWidth();
    }
    renderAlgorithm();
});

function orderWidth() {
    $('.ui.raised.container.segment.div_to_body').css('width', '100%');
    $('.ui.one.column.container.segment.ivprog_visual_panel').css('width', '100%');
}

function orderIcons() {
    $('.ui.one.column.doubling.stackable.grid.container').css('display', 'none');
    $('.only_in_frame').css('display', 'block');
    
}


function inIframe () {
    try {
        return window.self !== window.top;
    } catch (e) {
        return true;
    }
}


function full_screen() {
    // check if user allows full screen of elements. This can be enabled or disabled in browser config. By default its enabled.
    //its also used to check if browser supports full screen api.
    if("fullscreenEnabled" in document || "webkitFullscreenEnabled" in document || "mozFullScreenEnabled" in document || "msFullscreenEnabled" in document) {
        if(document.fullscreenEnabled || document.webkitFullscreenEnabled || document.mozFullScreenEnabled || document.msFullscreenEnabled) {
            var element = document.getElementById("ui_main_div");
            //requestFullscreen is used to display an element in full screen mode.
            if("requestFullscreen" in element) {
                element.requestFullscreen();
            } 
            else if ("webkitRequestFullscreen" in element) {
                element.webkitRequestFullscreen();
            } 
            else if ("mozRequestFullScreen" in element) {
                element.mozRequestFullScreen();
            } 
            else if ("msRequestFullscreen" in element) {
                element.msRequestFullscreen();
            }
        }
    } else {
        $('.expand_button').addClass('disabled');
    }
}