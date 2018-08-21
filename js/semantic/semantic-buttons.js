
var button_ready = function() {

  var
    $buttons         = $('.ivprog_format .button'),
    
    handler = {

      activate: function() {
        $(this)
          .addClass('active')
          .siblings()
          .removeClass('active')
        ;
      }

    }
  ;

  $buttons.on('click', handler.activate);

  $('.ivprog_format_info')
    .popup({
      popup : $('.custom.popup'),
      on    : 'click'
    })
  ;

  $(".data_types_dropdown")
    .dropdown()
  ;

  $('.ui.dropdown')
    .dropdown()
  ;

  $('.program_signature_text').text(i18n('program'));


  $('.add_function_button').on('click', addFunctionHandler);

  // Atualiza a tela do algoritmo
  renderAlgorithm();

  $('.ivprog_visual_panel').removeClass("loading");

  //Sortable:
  Sortable.create(listWithHandle, {
    handle: '.glyphicon-move',
    animation: 100,
    ghostClass: 'ghost',
    group: 'functions_divs_drag',
    onEnd: function (evt) {
      updateSequenceFunctionHandler(evt.oldIndex, evt.newIndex);
    }
  });

};

// attach ready event
$(document)
  .ready(button_ready)
;