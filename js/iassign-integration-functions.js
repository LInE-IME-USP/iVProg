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
        
    }
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


// Função para que o iMA leia os dados da atividade fornecidos pelo iTarefa
function getiLMContent () {

    // O parâmetro "iLM_PARAM_Assignment" fornece o URL do endereço que deve ser
    // requisitado via AJAX para a captura dos dados da atividade
    $.get(iLMparameters.iLM_PARAM_Assignment, function (data) {
        
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
    var table_el = '<table class="ui blue table"><thead><tr><th width="30px">#</th><th>'+LocalizedStrings.getUI('text_teacher_test_case_input')+'</th><th>'+LocalizedStrings.getUI('text_teacher_test_case_output')+'</th><th width="80px">'+LocalizedStrings.getUI('text_teacher_test_case_actions')+'</th></tr></thead>'
            + '<tbody class="content_cases"></tbody></table>';

    div_el.append(table_el);

    div_el.append('<button class="ui teal labeled icon button button_add_case"><i class="plus icon"></i>'+LocalizedStrings.getUI('text_teacher_test_case_add')+'</button>');

    $('.button_add_case').on('click', function(e) {
        $('.content_cases').append('<tr><td>88</td><td>more data</td><td>my data</td><td>more data</td></tr>');
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