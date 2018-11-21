
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

  // Atualiza a tela do algoritmo
  //renderAlgorithm();

  $('.ivprog_visual_panel').removeClass("loading");

  $(document).mousemove(function(e){
    var parentOffset = $('.ivprog_visual_panel').offset();; 
   
    mouseX = e.pageX - parentOffset.left + 300;
    mouseY = e.pageY - parentOffset.top + 100;

    /*$('.created_element').css('top', mouseY);
    $('.created_element').css('left', mouseX);*/

  });


  //Sortable:
  Sortable.create(listWithHandle, {
    handle: '.glyphicon-move',
    animation: 100,
    ghostClass: 'ghost',
    group: 'functions_divs_drag',
    // onEnd: function (evt) {
    //   updateSequenceFunctionHandler(evt.oldIndex, evt.newIndex);
    // }
  });

};

var mouseX;
var mouseY;

// attach ready event
$(document)
  .ready(button_ready)
;