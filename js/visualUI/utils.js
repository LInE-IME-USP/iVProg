
export function renderErrorMessage (dom_obj_target, message_text) {

	dom_obj_target.popup({
		html : '<i class="ui icon inverted exclamation triangle yellow"></i>' + message_text,
		transition : "fade up",
		on    : 'click',
  		closable : true,
  		movePopup : true,
  		boundary : window,
  		preserve : false,
  		target : false,
		className   : {
		  popup       : 'ui popup invalid-identifier'
		},
		onHidden : function($module) {
			dom_obj_target.popup('destroy');
		}

	}).popup('toggle');

}