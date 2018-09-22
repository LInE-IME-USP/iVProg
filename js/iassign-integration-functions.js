// Função para ler parâmetros informados pelo iTarefa via URL
// Apesar de não ser obrigatório, será muito útil para capturar os parâmetros
function getParameterByName(name, defaultReturn = null) {
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
function getAnswer() {
    // Se o parâmetro "iLM_PARAM_SendAnswer" for false,
    // então trata-se de resolução de atividade
    if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
        // Montar o retorno da resposta do aluno
        
    } else {
        
    }
}

// Função chamada pelo iTarefa para receber a nota do aluno na atividade
// O retorno é um valor entre 0.0 e 1.0
function getEvaluation() {
    if (iLMparameters.iLM_PARAM_SendAnswer == 'false') {
        // Calcula a nota do aluno:

        // A chamada do método abaixo é obrigatória!
        // Observe que a chamada parte do iLM para o iTarefa
        parent.getEvaluationCallback(0);
    }
}


// Função para que o iMA leia os dados da atividade fornecidos pelo iTarefa
function getiLMContent() {

    // O parâmetro "iLM_PARAM_Assignment" fornece o URL do endereço que deve ser
    // requisitado via AJAX para a captura dos dados da atividade
    $.get(iLMparameters.iLM_PARAM_Assignment, function (data) {
        
    });
}
