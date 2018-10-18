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
localStorage.setItem('ivprog.lang', iLMparameters.lang);

// Função chamada pelo iTarefa quando o professor finaliza a criação da atividade
// ou quando o aluno finaliza a resolução do exercício
// O retorno é um JSON com os dados do exercício ou da resolução
// Esse retorno será armazenado no banco de dados do Moodle, pelo iTarefa
function getAnswer () {
    // Se o parâmetro "iLM_PARAM_SendAnswer" for false,
    // então trata-se de resolução de atividade
    if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
        // Montar o retorno da resposta do aluno
        
    } else {
        // Montar o retorno com a criação da atividade do professor
        return prepareTestCases();
    }
}

function prepareTestCases () {
    var ret = '{ \n "testcases" : [ '
    var test_cases_array = $('form[name="test_cases"]').serializeArray();
    for (var i = 0; i < test_cases_array.length; i = i + 2) {
        ret += '\n{ ';
        ret += '\n "input": "' + test_cases_array[i].value + '",'
        ret += '\n "output": "' + test_cases_array[i+1].value + '" '
        ret += '\n},'
    }
    ret += '\n] }';
    return ret;
}

// Função chamada pelo iTarefa para receber a nota do aluno na atividade
// O retorno é um valor entre 0.0 e 1.0
function getEvaluation () {
    if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
        // Calcula a nota do aluno:

        // A chamada do método abaixo é obrigatória!
        // Observe que a chamada parte do iLM para o iTarefa
        parent.getEvaluationCallback(0);
    }
}


var testCases = null;

// Função para que o iMA leia os dados da atividade fornecidos pelo iTarefa
function getiLMContent () {

    // O parâmetro "iLM_PARAM_Assignment" fornece o URL do endereço que deve ser
    // requisitado via AJAX para a captura dos dados da atividade
    $.get(iLMparameters.iLM_PARAM_Assignment, function (data) {
        // testCases é preenchida
    });
}

// Função para organizar se para criação, visualização ou resolução de atividade
function prepareEnvironment () {
    if ((iLMparameters.iLM_PARAM_AssignmentURL == "true") && (iLMparameters.iLM_PARAM_SendAnswer == "true")) {
        prepareActivityCreation();
    }
}

// Função para preparar a interface para o professor criar atividade:
function prepareActivityCreation () {

    $('.add_accordion').addClass('accordion');

    $('.default_visual_title').toggle();
    $('.default_visual_title').append('<span>'+LocalizedStrings.getUI('text_teacher_algorithm')+'</span>');
    $('.height_100').removeClass('height_100');
    $('.main_title').remove();
    $('.ui.accordion').addClass('styled');
    
    $('<div class="ui checkbox"><input type="checkbox" name="include_algo" tabindex="0" class="hidden"><label>'+LocalizedStrings.getUI('text_teacher_algorithm_include')+'</label></div>').insertBefore('.content_margin');
    
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

    //return JSON.stringify($('form[name="elaborar"]').serializeArray());


    var table_el = '<form name="test_cases"><table class="ui blue table"><thead><tr><th width="30px">#</th><th>'+LocalizedStrings.getUI('text_teacher_test_case_input')+'</th><th>'+LocalizedStrings.getUI('text_teacher_test_case_output')+'</th><th width="80px">'+LocalizedStrings.getUI('text_teacher_test_case_actions')+'</th></tr></thead>'
            + '<tbody class="content_cases"></tbody></table></form>';

    div_el.append(table_el);

    div_el.append('<button class="ui teal labeled icon button button_add_case"><i class="plus icon"></i>'+LocalizedStrings.getUI('text_teacher_test_case_add')+'</button>');

    $('.button_add_case').on('click', function(e) {
        addTestCase();
    });

}

function addTestCase () {
    var new_row = $('<tr><td class="counter"></td><td class="expandingArea"><textarea rows="1" name="input"></textarea></td><td class="expandingArea"><textarea rows="1" name="output"></textarea></td><td class="btn_actions"><button class="ui icon button"><i class="red icon times"></i></button></td></tr>');
    $('.content_cases').append(new_row);

    new_row.find('button').click(function(e) {
        new_row.remove();
        updateTestCaseCounter();
    });

    $('textarea').on('input', function(e) {
        var lines = $(this).val().split('\n').length;
        $(this).attr('rows', lines);
    });
    
    updateTestCaseCounter();
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
    div_el.append('<div class="ui stackable five column grid">'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="integer_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('integer')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="real_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('real')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="text_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="boolean_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('boolean')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="void_data_type" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('void')+'</label></div></div>'
        +'</div>');


    div_el.append('<h4 class="ui header">'+LocalizedStrings.getUI('text_teacher_commands')+'</h4>');
    div_el.append('<div class="ui stackable three column grid">'
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
        +'</div>');

    div_el.append('<h4 class="ui header">'+LocalizedStrings.getUI('text_teacher_functions')+'</h4>');
    div_el.append('<div class="ui stackable one column grid">'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_teacher_create_functions')+'</label></div></div>'
        +'<div class="column"><div class="ui checkbox"><input type="checkbox" name="" checked tabindex="0" class="hidden small"><label>'+LocalizedStrings.getUI('text_teacher_create_movement_functions')+'</label></div></div>'
        +'</div>');

    $('.ui.checkbox').checkbox();


}