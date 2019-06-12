import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as VariableValueMenuManagement from './variable_value_menu';
import { registerUserEvent, ActionTypes } from "../../services/userLog";
import WatchJS from 'melanke-watchjs';

window.timer = false;

export function renderExpression (command, function_obj, div_to_render, expression_array) {

	div_to_render.empty();

	WatchJS.unwatch(command, "expression");
	WatchJS.watch(command, "expression", function(){
		if (window.timer)
			return;
		var m = div_to_render.find('.single_element_expression').not('.mouse_distance').not('.add_parentheses');
		var s = "";
		m.each(function(e){
		   if ($(this).hasClass('parentheses_in_expression')) {
		   		s += ($(this).text()) + " ";
		   } else {
		        s += ($(this).find('.text').text());
		        
		        s += ($(this).find('.var_name').text());

		        s += ($(this).find('.parameters_function_called').text());

		        s += ($(this).find('.value_rendered').text());

		        s += " ";
		   }
		});
		if (s) {
			window.timer = true;
		} else {
			return;
		}
		registerUserEvent(function_obj.name, ActionTypes.CHANGE_COMMAND_EXP, command.type, '/', s);
		setTimeout(function() {
			window.timer = false;
		}, 200);
	}, 20, true);

	if (command.type === Models.COMMAND_TYPES.attribution) {

		WatchJS.unwatch(command.variable);
		WatchJS.watch(command.variable, function(){
	      renderExpression(command, function_obj, div_to_render, expression_array);
	    }, 0);

		if (command.variable.content) {

			var types_included = [];

			if (command.variable.content.type == Types.INTEGER || command.variable.content.type == Types.REAL) {
	    		types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		    } else if (command.variable.content.type == Types.BOOLEAN) {
		    	types_included.push(Models.EXPRESSION_TYPES.exp_conditional);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_logic);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		    } else if (command.variable.content.type == Types.TEXT) {
		    	types_included.push(Models.EXPRESSION_TYPES.exp_conditional);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_logic);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		    } else {
		    	console.log('NÃO RECONHECI! VEJA: ', command.variable.content.type);
		    }

		    renderElements(command, function_obj, div_to_render, expression_array, types_included);
		} else {
			div_to_render.text(LocalizedStrings.getUI('var_menu_select_var').toLowerCase());
		}
	} else {
		var types_included = [];
		types_included.push(Models.EXPRESSION_TYPES.exp_conditional);
		types_included.push(Models.EXPRESSION_TYPES.exp_logic);
		types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		renderElements(command, function_obj, div_to_render, expression_array, types_included);
	}

	div_to_render.children('.mouse_distance').addClass('mouse_distance_hidden');
	div_to_render.children('.higher_element').on('mousemove', function(evt) {
		if (!window.open_or_close) {
			$(this).css('position', 'relative', '!important');
			$(this).children('.mouse_distance').css('opacity', '1');
		}
	});
	div_to_render.children('.higher_element').on('mouseout', function(evt) {
		if (!window.open_or_close) {
			$(this).css('position', 'absolute', '!important');
			$(this).children('.mouse_distance').css('opacity', '0');
		}
	});
	var lixeira = $('<div class="lixeira" draggable="true"></div>');

	div_to_render.find('.single_element_expression').on('mousedown', function (evt) {
		window.posX = evt.clientX;
		window.posY = evt.clientY;
	});
	
	Sortable.create(div_to_render[0], {
	    animation: 100,
	    ghostClass: 'ghost',
	    group: {
	        name: 'shared',
	        put: false // Do not allow items to be put into this list
	    },
	    draggable: '.single_element_expression',
	    sort: false,
	    filter: '.not_allowed',
	    
	    onStart: function() {
	    	$('body').append(lixeira);
	    	lixeira.css('display', 'block');
			lixeira.css('top', window.posY + 70, '!important');
			lixeira.css('left', window.posX - 20, '!important');
	    },
	    onMove: function() {
	    	lixeira.addClass('color_test');
	    },
	    onEnd: function() {
	    	lixeira.remove();
	    	div_to_render.find('.ghost').removeClass('ghost');
	    }
  	});
  	new Sortable(lixeira[0], {
	    group: 'shared',
	    animation: 150,
	    onAdd: function (evt) {
	       lixeira.css('display', 'none');
	       lixeira.find('.single_element_expression').remove();
	       lixeira.css('background-color', '');
	       lixeira.remove();
	       removeElement(evt, expression_array);
	       renderExpression(command, function_obj, div_to_render, expression_array);
	    }
	});
}

function removeElement (event, expression_array) {
	var indice = $(event.item).data('index');
	var first = expression_array[0];
	console.log('indice: ', indice);
	if (expression_array[indice].type) {
		// if is alone in expression:
		if (expression_array.length == 1) {
			//function_obj.commands.splice(function_obj.commands.indexOf(command), 1);
			expression_array.splice(0, 1);
		} else if (expression_array.length > 1) {
			if (indice > 0 && expression_array[indice - 1].type_op) {
				if (indice < (expression_array.length) 
					&& expression_array[indice - 2] == '('
					&& expression_array[indice + 1].type_op) {
					expression_array.splice(indice + 1, 1);
				}
				expression_array.splice(indice, 1);
				expression_array.splice(indice - 1, 1);
				if (indice - 2 < (expression_array.length) 
					&& expression_array[indice - 2] == '('
					&& expression_array[indice - 1] == ')') {
					expression_array.splice(indice - 1, 1);
					expression_array.splice(indice - 2, 1);
					if (indice - 3 >= 0 && indice - 3 < expression_array.length
						&& expression_array[indice - 3].type_op ) {
						expression_array.splice(indice - 3, 1);
					}
				}
			} else if (indice < (expression_array.length - 1) &&  expression_array[indice + 1].type_op) {
				expression_array.splice(indice + 1, 1);
				expression_array.splice(indice, 1);
			} else if (indice < (expression_array.length - 1) && indice > 0 
				&& expression_array[indice -1] == '(' && expression_array[indice +1] == ')') {
				if (indice > 1
					&& expression_array[indice - 2].type_op) {
					expression_array.splice(indice + 1, 1);
					expression_array.splice(indice, 1);
					expression_array.splice(indice - 1, 1);
					expression_array.splice(indice - 2, 1);

				} else if (indice < (expression_array.length - 2) 
					&& expression_array[indice + 2].type_op) {
					expression_array.splice(indice + 1, 1);
					expression_array.splice(indice, 1);
					expression_array.splice(indice - 1, 1);

				} else {
					expression_array.splice(indice + 1, 1);
					expression_array.splice(indice, 1);
					expression_array.splice(indice - 1, 1);
				}
			}
		}

	} else if (expression_array[indice].type_op) {
		// iVProg doesn't support operator remove
	} else {
		
		var opening = -1;
		var closing = -1;

		if (expression_array[indice] == '(') {
			opening = indice;
			for (var i = indice + 1; i < expression_array.length; i++) {
				if (expression_array[i] == ')') {
					closing = i;
					break;
				}
			}
		} else {
			closing = indice;
			for (var i = indice - 1; i >= 0; i--) {
				if (expression_array[i] == '(') {
					opening = i;
					break;
				}
			}
		}

		if (expression_array[opening + 1].type_op) {
			expression_array.splice(closing, 1);
			expression_array.splice(opening + 1, 1);
			expression_array.splice(opening, 1);
		} else {
			expression_array.splice(closing, 1);
			expression_array.splice(opening, 1);
		}
	}
	// if expression is empty, add a new var value:
	if (expression_array.length == 0) {
		expression_array.push(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
	}
	if (first != expression_array[0] && expression_array[0].type_op) {
		expression_array.splice(0, 1);
	}
}

function renderElements (command, function_obj, div_to_render, expression_array, types_included) {
	/*if (expression_array.length > 0) {
		if (!expression_array[0].type_op) {
			renderStartAddOperator(div_to_render, types_included, expression_array, command, function_obj, 0);
		}
	}*/

	var i = 0;
	for (i = 0; i < expression_array.length; i++) {
		if (expression_array[i].type == "var_value") {
			var div_temp = $('<div class="single_element_expression" data-index="'+i+'"></div>');
			if (i == 0) {
				if (expression_array.length > 0  && !expression_array[0].type_op) {

					//renderStartAddOperator(div_to_render, types_included, expression_array, command, function_obj, 0);
				}
			}
			VariableValueMenuManagement.renderMenu(command, expression_array[i], div_temp, function_obj);
			div_to_render.append(div_temp);
		} else if (expression_array[i] == '(' || expression_array[i] == ')') {
			if (expression_array[i] == ')') {
				renderFinalAddElements(div_to_render, types_included, expression_array, command, function_obj, i);
				renderParenthesis(div_to_render, expression_array[i], command, function_obj, i, expression_array);
			} else if (expression_array[i] == '(' && !expression_array[i + 1].type_op) {
				renderParenthesis(div_to_render, expression_array[i], command, function_obj, i, expression_array);
				renderStartAddOperator(div_to_render, types_included, expression_array, command, function_obj, i + 1);
			} else {
				renderParenthesis(div_to_render, expression_array[i], command, function_obj, i, expression_array);
			}

		} else {
			if (i == 0) {
				console.log("NEGAÇÃO NO PRIMEIRO ELEMENTO");
			} else if (expression_array[i - 1] == '(') {
				console.log("NEGAÇÃO APÓS O PARÊNTESES");
			}
			renderOperatorMenu(command, function_obj, div_to_render, expression_array[i], types_included, i, expression_array);
		}
	}

	renderFinalAddElements(div_to_render, types_included, expression_array, command, function_obj, i, true);

	renderAddParenthesis(command, function_obj, div_to_render, expression_array, types_included);

}

window.parentheses_activate = false;
window.open_or_close = null;
function renderAddParenthesis (command, function_obj, div_to_render, expression_array, types_included) {
	var addParentheses = $('<div class="single_element_expression add_parentheses not_allowed"><img style="width: 15px; position: relative; top: 3px; left: 1px; height: 15px;" src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAOgAAAEjCAYAAAA48U27AAAasXpUWHRSYXcgcHJvZmlsZSB0eXBlIGV4aWYAAHjarZtZkhw5km3/sYpeAkYFsByMIm8Hvfw+Fx4kg6zM7HolHaykOz3MzWA63EFh5c5//7/r/osfK8m7XGqzbub5yT33OHjT/Ofn8xp8fn+/n/Pjd+H3z11MX28jr4nX9PmFna/jB5+XX1+o+evz+fvnrq7Pm9i+TvT1C078fpKurPf7a5FfJ0rx83n4+rfrX18Y9u12vv6L6+u0Xyf/89+5EoxdOF+KLp4Ukv/8/blSYhWpp8Grvb9L1CeJ9+n9HVL71/g5/Wutvw7gz3d/xM//WFn6FQ73iezXAfZHnL4+D+WPz9PPy8TfVhTizyvH7yuyEZr//vMtfvfudu/53N3I5giXfd3Uj1t57zhwEs70vmb8qfxXeF/fn86f5odfZG1zq9P5yT96iMT6hhx2GOGG815XWCwxxxMrrzGumN5nLdXY43pJyfoTbqyO/OzUyNUic4mP48+1hHfdrutxscaVd+DIGDhZ4Bu//XF/fvCf/vntRPeqDkLw7RMnyoJ1RZUsy1Dm9DdHkZBwv2JaXnyD+7z4P3+U2EQGywtz4waHn59TzBJ+1VZ6eU6+OA7N/tMvoe6vExAirl1YTEhkwFtIJVjwNcYaAnFs5Gew8phynGQgFFfiZpUxp2Qkp0Vdm+/U8I6NJX4+Bl5IRKFpKqmhgUhWziUb/dYooeFKKrmUYqWWVnoZlixbMbNqwqlRU821VKu1ttrraKnlVpq12lrrbfTYEzBWXLdee+u9j8FFRx6ca3D84IMZZ5p5lmmzzjb7HIvyWXmVZauutvoaO+60gQC3bdfddt/jhEMpnXzKsVNPO/2MS63ddPMt12697fY7fmYtfLXtb1n7M3P/nLXwlbX4EqXj6q+s8XGtP04RBCdFOSNjMQcyXpUBCjoqZ76FnKMyp5z5HpNL4BarLErODsoYGcwnxHLDz9z9ytzf5s0R3f/fvMW/ypxT6v4vMueUum+Z+9e8/UXW9nhw+yEydSExBSET7cdBIzZWA1MBNoQnLSu+tlhBoVpHT7kH8KX0vLhLP2s0Cvr61M7xdR+XzqKg2rht2eGnzjt3PzeectfiZhrRJnRXnGmWeqz68rC5/T0gBidKNzRX2/BllkZgbl473ZpL6H53fTndlNpSUy6gjTscu/ZZRIuF/4kWdqrNzwUe8WbPRcAnd0Zci59mmYWUGnqD71jvukQszVvWSW2wsHnGui3M47tdv2/LrtmYZJHc+11KZmGA6lo11DQF87OtRAI5AyyQsg+ndwqje2qFFShDy2bYbrIc3uTc5g6cydRG2UfST/SlCf769RKsk5AyKRLyeR3XynM3i34OI4aUUAPiCSjHkS3uzOi0um+t5cx2k41LYR2+3/JWco8fc7sNxrU74febzi6QnmpkDH/SDGeullbNkaBE1kA8aYA21BidFqMdLLQ4qHyHpLCknvaLAp35hnWhTjhuQ3W1ndx2o75ZYxcAL0D0VhZFpIPdRp4PQHydNyK289mdAjI6f8509wEH6nqkQnXNFjbNfcOk5SYRqcsqSydFRGar+roLvebNre/FvQbUUgynkdmdRz91IGEW/7DmqVxascOXJGVpbay8iXFjbDW7HsSeE2x5K9/8R92RlUjR3jRi3aOudK2ONSj3swEYosIdLTXhruH2XZe7FViZCeAIjbR0An6BmMZHrexKzZK601F80eqsZLAfiqqXdMlt4H43qyjm4ljQkz9U2x7cAPdTYjulNJTDEZjdQyufDVI2muX4u200NfdWhsvhNbTrwJV9uBsfy2rT1Nwcdo/tsTdg5QtkTlNIMMbwwDEszkY15UCr62SURqUgF6GnrnoK0+omImDfAcWo0RYhi3qAjYys9CtXQkyt3NAJ4/le7u69OZuiKNw0nQjs2bZpO64qONwNKOFNo4CR6wOMArG5y2sLPOxUSCmSfgjL5QFC8I8bDqVPMuJtAbMsDxS/xWqIi0YiTlRBjpSpn5DLLUgv1n4AJ2eUiB9vjf/wenum9TprPoTzgjzlltxQcUb7D+rEFc8r51+DNs1n3mxAz7nw4t5UM/hSIxAY70J3UBZzcaJN8CO33EgOt3g2WVNhEgAliw6Fx1YQ7NxQAUGKEKKgFiA+r5DTop6SVPIzzRLBoOvPnMcFOsSPA/NXoYM4hSWSypoA896oaE5Ookal4WKcVBGZOYTxtk6XAcEnjunoo7FXSnTgZSkZDj5GT4IX1G5l5RmWf/W+VVo5URhg+i4TOEZO0Vkdq+RCW0N9RsyjJd1HXT2Jj3KjndQCZ3RbIv65BrmBN9ZeKkQOV+TOpDoREVTnHplqKUlgwc/y/SRrZd3RyyFfTd2/LJZGFSAkAIgMpKVBMqOWGZYrFa1BpsYCZDuVh/Hw7dR0pkJOyy2jktPqh45q9O2MhaXFNNecWleGy1p3yWYEmULvFFb2hOSACiDYqqxmTGxAXcUSyyN8m+uPGgUqRcVFNYeZSc1x75xJfEUfdwl+qnx8p41ZLxFiPf0SshMn58NkrPYoFAZNyIbgAH5KiMuS3AaoDvVLPRfonSuNAaoY5QiCj77boq8NWAYb+fpGLKsGPX2PFzlHodwXT9XV8KkZZIiJaWpGhFq+/0xx79V95zxWB6FepYuTnQgg+IaGI98mVKWNGyAWIRBQ/f2p1GAAbqejbSCJQaI5ydFJbCaamxI/kSDXgqTrFWJIaDE8+qqUB3Cy6aVLUR60U1zbSVHcgkJZH6xDHE4o1IygnGieqyEGxfMIAXofYTSVWgJR0QHrKnuVFpGqsM1XOjhPHaJs4t0xS2bEPCZCrbDk6PFziDQAAjTiJBPseLJMkAowO6rjwiJoLrhwokWopzCRqVOEg0AMWulGD6sbNnGDvWjElxb+4s6oN4IN9yNGYN9dUoZao52IVsEpdUnoSx0+UEGr4IFDiQ9KEpjWgbwUVM2DWnaT2tf7ghLGvD6dZ2qZQ73GjPjsCen00WdfOg1B1vaxXigRGp5ujMEVaSJ14ubALTifcDeozb03lhhoQfTtjM+hUZGZRpxUN5naxBuZFERs7kwxMNp1b+QSij9cgGDkPShn6AcctIY1N/nrCn48FUemrX7XktmV3xb9d6+lgKz5zgqe5TX7Tuds2mHuBXkFVLHLT/cYSlN1DEgFhY7b2PuePqNyNhbqv3cWPSkcIL5kBC+iZkPt3M9d9brtDQGnfugbpgF8NqkFPUs7nwGW7BBNjN6hZmeRHiWun5ViGKCANhe3xiXR9DT6TiAbbY4+seUl3jqdxCIQ0hJy5BI9SEWNg4yDWJEdHg6AH+lD1zduLpvYX30Fvx1ACQz1ZbCyFJK4AO0NDmhcBSVD2XvQJ+Nu7KyKjGp38jp3Js4Txiy4AKXroEzAZd9B9+03YF2feDlrUu0wAxAFw1C3aJiL7k3XjYK34LR030sRGfq8BrX1TNAGDE83gQkRjlwxIk9NoM21N5VxLram4WnpKVYJo4A8c2GvEDKN66EqUL/5iO4q5D1REbNFMARu8wAEMS2h4Cmg+eH+F+D7q9cp4jOFGpdEG+AkS3T8ljYDHfpKE74lZ7fbvhHhfQ0XA8dt4XiRWLqAFFYEDPKFm8Vn3I6ptImq5UqAVqWPqSgaclICpsY+YX/JGizHSmUQJXRKtEejInIMKuIwnxxw2UB10m1Lzd2JoqOCj5BOINdlFNKgvS1KQBx5golqCLiL8WYJCWknKHU9ofBx6bPyrV5BNeS3EVO6QMNO1OAAGPB7NALsG9TUNdP/Xg4H6OZ7SDc3aClYF26EX3elFeGxOXPdAYPMxaljEn6VW3w6TRI3MlLnxsL+aO0DjHDd7xBlUnAjoRe6BsWQHt5z424QUiwAQXxuTsATMLsWNp8eXdkqsibNOWBk1bpmBnQ/eiJLL2B1YCZNGmD8jcsqBfnVNOhdwEP4Xhb/QR1dAWeuA6Mx8CtSvTseFzWpQCpezB3VjP5ba2AhUOg0OVDEr7LEFcte2xBeJV7hdfdj42uoNvRcrA5bYF4ThrpkwjyKrc/9aBNBjTajfnBs+JuqlMQgbZgk+9AhaL6K/SUp3XFtg9H5oICbVN6VR+r3iax2SW3r6PUMyhS6irsos3JC0jo0uQaZ8LAXWUNdKJeA6sIe1osSg64w/gcUOm1zGPp410syLwIBy08mep6qWs350EzgkxPuAsTwsCEbB9jExWkTLCDV0KlrAxMewZSj1V18FaX0TLQBTJqswIBu8KUCrABy9CuW6+UGFoWf6TcPX65UATPEJhFGBeADKLMCPVXlhUwgaHCQBwleqaAG9WnYA5thjMbAxCCoeTuxr8imVbDdROMKSGY99WnV9AMO3R+4+O++hotSIO1BUpgK7m5kgZOdjJPs+oyF3MoLVoVuItRWj/QG2l+OeEcKCmOIQJmiN0RSWShmNyGdWelM4CV1MReqFLU/T/3AYU4Z80vwK0Att2AjLsgWH09MdsaCHA1ZHir1hUduHCafuTUTglsznzQh1JYVyfT11D7NQmfTr01CGy6k+jFw40Et4nGBDsApGh9pSIYzgvkjsUADEBp4p5hOupTAFHtFgKUuurqhyrhJrOilJLqpThRCzS7/o+i7XzQWuVSm4N6qISOKrj/ZJzV3BSH7rw95Bzgd8Uu+YEK2LEMd8C8JuOiFCBDaGxYA1F6vE6TsJaOsChYrHYl+V9HcOg3yRNoCraJqWxKR8DJWjYaa3mqEEqwZFIG/IeUBz5o0BcBAgEndIVSQeOA1/EoB34thle6+05f8bm0k0+4dXvT8A+S5v/kFxQg8HY2TML9UJOqCrqv8G55tpF/sERqW8UJVO7rZyForA2KBpS5E+voZOnmvyEkkL+KbuuOMWN2F2R3aaaEINjoVbM2cwf3TKaj4InhG1wB3pukv+m+gp1gv8nAi6yB3j7X2DurCN2+0GiWrzYutkR6RsohtwOmxeIqMwt7gx4Emn2dpMsQaLnPX7xD3xzG/jsCU1DfToCpQFxRJ1pyWk9z8/PvvJeF+1kSQ+GHh6D3YICMrwSTxe4HsybMGCp+03a6BWwb58msimPwkZ4BfKobJPbSRlZ63rNkyCej+kOxtRqBYuqEAIRrM8doCobQJUjaueaozze8bDhGBh3a2z3wv/6/zm99fU3N2icLVjK8MNc9uH1uFNMXukAUU0esuJeRq0i4s8KaJEzc+F3yxd6+APxhCilGWqgSv7yylXadDZ/MGLzPWoXZgA3JjlN/LHMSpXT1x6sAdoTm5abwnuMiH5FIDAPj9kBCocbYWuhmSDmYoaFFKH3wlM3AgRcp/lNl2dNKNGgUHAlg6Kq1P9M8KhgpOxKtYhdQX+F27DEOnN8/GTinxoLZuNQbS/yhuaHhB+YFrmg6N10MBGT2GXz1gKxOBSSwii+CGrG9GbmgcfDCn9Ti0yf7CEaItwQ9MCpw0HLnG92RUKe7ONy8qDq2IR2U1CbmNekgRSPLDUb6ULNqEEpC5fQpIN9CxaBzk8cEywTJjtACGpeVaHmjieEc6I9LBjRjhWUgwIeTQ2UvQ+f69UeCPEnoV9KY1vxXR35YQOP8iyx38axkN97dl9FVEXPlTRnOPlOkJhE0nmKhzdBY2TGYUtHCJHPKDj0M8fYQskN6wmCdA36FzXiShRtbopdEiJK6x2o17robiKJLEcXGihGUAOzVXJa6I9xPj0IRuQMoUw7h1DjQwiJD2wS1RpT6HxsWRYBhW6dLtClDRMqCpWsJMdFu4DWyLYT3BNoq0JYAs6Bt0NdJqoakIkLgam9nRh3cSoxG1rYefQaWnFoOSizL7rA2ttmCtiaaaODPgJInOub/tz/qQxR0FyeLykwDfFcBXVo/y2OHP3oRJSFlkV28tI2vK21hR/yAW+z4Ukju6G/N0EgDFgiVde/8gX1AFUAnxaHwgn0ZIkCAZKYu81WfYDozPbubQz+CBVNT6q0kEIkriflm3TvOg51OmqAyrDxBqwIWVJy/HsZw3NATZUf50AapGaj0N7U1QZU2XxglrwwckME1w0L3cnLR5WHn1SYYcbgHlQ7dJHVMLHPrwAOFZuEiUvjKkEQ6Ptt0iU1LGa6WQsF5wx0ENFBQbN00P4xLEjJqtIUGyjN7ZT2qRvvN2iAztQQKWkAczqd+1dwhiXxrynKLfX7WHTW4ev0MpwK8aH9MYFA7X1RswEry1OV/8UWxBGWEJVDYuBXhMHmm3tOHBl6emC0IklPUo+p/SvJ/13FMbbTQm5YYNWJ1DqC5MDURHdyQac2qzcVDfJFlb4fQVcaoEAEnWg6qISkCVVhnV/kKEAXlEMI+kHzj4AVy8VnsBxHYQAgs69Eq7eZqWGy6ajKrYABtbbxca+UnTWHbAsaZejbp7BJzgp68zfzsv3abNfQoeXaaReFxcJ1b+gPXt5OWK9kQR1MRA2w77hdq/LO43qtd20Rk4b67EjWCUh/yhjBJ6ARD3EkrEiCRj9NGgx3S7tFysYXbNmVEOmFRgRGwXtUMWRQgkaeCQITpIa9aI5wbYoBZ8eH17mVgOlnUDqjS2H7J5brwWVh9dALYl6HMQZkMFZnqLxuGyAIrbaBRoUU5KKLaRHVvcaDmnjkqBPw7ldbCu2BQMHIHT80IqGmAf0J00DVkj1Je+SChcjfaoAyEuyiiAa/IsELR88e/TRKMckQFoSxRoEbZryJK1PZomaB/gWPRkS/F8ZEKbH6Gnjdoq2iC/l+4HE8C5qZ3DLwRzc2vi9m8PEbrH0MruYRJGJR0rFQnX6rRZSV1tPFkhchEopaj0MEb3b/MEyAO9LkRAb3VkWwfHQAPcK/kHPvGEaB2n/eS0C5JPfbY8AKmNN4VuD/wz1W0ekU0/kY1M5vrcECGttjpeXQ80YDqj85am8p/0jAFOsra+fxgoDQO4NERG62ryQehy1lMJVhrLYeUwJIhneBHkTAFIyBTthPTe2jKjCMGQQXFDlW8zaHSLINIx6uI9TdQXzhNZQuFDPumi/KU+lqGEirRF1b4qZ7EKKY5MMUzN91G0qOak7WV+UymgUva3LAy3/ynhHXm4/TvE+6+DPof8OIClz9gpKwdGXw05NdKJ4BHFqT0TDf5or904WJt/+kI65+dvPp/7r09x4q7ngfiI6LmSbK+KCodcJ8psk2X6m/oWiGVMTh+w+YxohTww5KAemENzPgvRtR0tERNTxrYaZhxMTmm8rVCRr/pqa+wBYj/fFCRuYLm6S8IeWSo4IccxQawHa+VOy+O3PKuRupYsAX9FhcPC1qMgOQJ34AxkPzWpJ2FBOnctd1BfWiIa6dhZKNV9tUMGqGf+WWA503BeZEAlIqUpNcyRtr2bxZ5oG8NKucQxY68dEWLaziPSFE7A5KCw6BYkFWhETdaA6eVe1zoUIi4SFirAXkEf7DEc0pSqxczst58X0pSPr8gbTeFw1Z7O7Nr7L2VS5Agv3zU7qW/ChaLpevJhOCRcxGLrwZyFe00ShQVRpx2eBt7OTxcCe8j5RF60LxxvowGgfCJveqDlwv23PX0b9UARgLT1tAgcCKg16gI38X2+ALmTGQ3wkMGsAAjxEg/LQ0dyC3r2JLSOTjTRek3vISJ6j0rQzJ73NLUkA1gCXyHaKXoLYy0PCwM7GtVHYk+7EQuPjqWX2oFTkfv0x4RgatdDn6gVVCkNEVfnHWC6Zpga0xdFEi9y3vZz/ZJ8iBh6sO+EpffwWRA2f7XVSK35BYA1hHHSzFmPoPQKdk+XA3456ymzSbQXugnz1SX4OU5pyVvPniXkPSe0maFJvaJ5P4O68sThr2Fdex0XgaWMncF2Uwmosvh5aCf+7KlvLYUWKJ2be5NIx82QUaoxDO0RkwPq7QAcS9NwHOI+7ykvDErUeKwTKu0Doq2ORsifp0eWwSJyV7r0yPGg6jLCdyQ/qwYVNwKkXc81QMOz8l1Sqi1UeGLO52O0HYDWckhmwPNj9Qxpif+RPChqtXYjHlFj04wFAnXoNYInB/BAEIskOuV/qTs9OpOQGHpgpGhGCWUQk6anGOhubaNMkO3u5wSq8peCBNLYZcbNbQMaOCgE+yC4h6JLBxWqJ/7IsLY/3+Zpk22kX99DR7jaZ7x46SklPZgEtOBOiIvm2RVLaXHODAClqefrNNMB46ES9Dz9PULAH2x7vaiBZTVt+Ohxifd0D+Xl9PTV1OM6zQZuBKaNEiUggl8IkojUxq3Xq+0GAcDQbJ41ovaQoVyWTAEL3rXaVHwoGzB7In8KgkgO/YAAoyCCaFTUrlp2JY3r9QAf8SLo5waiSIVg6/XoOWUfPEUx3wb3rYXuBsWSniKBFge64SIqIeaQgacWjyeFsVmmqrP+yU26o82/g0nA7X/4OaHntH9KgGESIAkX2KwPzYFWgFq0CXA02NHRGhrLTTnVf8/aD3r+Od5nG5bo8GrOtjVvhjGADWEPmpFzTk35fdMOUtGeIqgN9yuWjZsDish2EA29saMMYtOuA0aNUPB69CzLMT3rA5nLo4ZGxhb2OLugh4T1QBHZEoBEgTrr5x/FtBGBm4JWpRvxECjNQ1/DLW1YBwq49I42REeEVs/cNNHLVEpK0VYfSB370wEUtNT/fTtxehAOXrE5MMOo+gHVXxZ5XYUEWOmTCRKu+DdZKtT6J2pH+u7zMGj+2K6/PrHTmZFCoNs7sWbAxdrXoAM/FDX61R4XzFHPBmejdHxRE0MfiL+lmcBwkK/cPNUtoXWh01W69mKgPm0vPXuHPBsbINva9FR5kHoZbc3mDvDa9EAcYH9Mj1NphF/04EddtDlYCXN3aATYwe8iDjwcpL2BqF6cVqGENMbGfpUNi+hhJD15kXpASmqIFkIpA0+WsVqdLszEXNH7PNuTiB/Vg/Oqg/vS4ASl+ClIjZo/yEttocbuwX97Pc82QJwDnIMWiImsjYuZc2ifspZe69qZAwsdQRCzvHxAmPXqwUcKTVu8UaERSkOBVfYKkb+XJEhqtExGMXNm/X8ZRnHBss+yI5Y+o8zOK8aV+G2EFIKFkAB2VTN8SKj3ni2gUE1EhBjCN2A8ktMOPQV/QugDsc0dY4/CM+aqGQRZ258pFrfI4jEzS0yUMObYTQilyYMpRgSdGuydXNdrQxa/qM5whf8DWb3GXCqgM7cAAAGEaUNDUElDQyBwcm9maWxlAAAokX2RPUjDQBzFX1NrRSoKBhRxyFCdLIiKOGoVilAh1AqtOpiPfkGThiTFxVFwLTj4sVh1cHHW1cFVEAQ/QFxcnRRdpMT/JYUWMR4c9+PdvcfdO4CrlxXN6hgHNN02U4m4kMmuCuFXdCKEPvAYkBTLmBPFJHzH1z0CbL2LsSz/c3+OHjVnKUBAIJ5VDNMm3iCe3rQNxvvEvFKUVOJz4jGTLkj8yHTZ4zfGBZc5lsmb6dQ8MU8sFNpYbmOlaGrEU8RRVdMpn8t4rDLeYqyVq0rznuyFkZy+ssx0msNIYBFLECFARhUllGEjRqtOioUU7cd9/EOuXySXTK4SFHIsoAINkusH+4Pf3Vr5yQkvKRIHQi+O8zEChHeBRs1xvo8dp3ECBJ+BK73lr9SBmU/Say0tegT0bgMX1y1N3gMud4DBJ0MyJVcK0uTyeeD9jL4pC/TfAt1rXm/NfZw+AGnqKnkDHBwCowXKXvd5d1d7b/+eafb3AxZ8coJmdd3gAAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAC4jAAAuIwF4pT92AAAAB3RJTUUH4wYFECEioUMxaQAAHgNJREFUeF7t3Xm4nVV59/HvOgMZSMKQwXAQODs0QmQKU2QeLCAiin0FqW/r++JQx2odKwJqqx5RK6iFFrWVCh0EHAEVWuDibRVQKK+nKMrFYMIUpkAgA2Q4yeof+xx6kpyz72evda+19nB/rst/cn4neO08v+d+9jOsx3nvyck5J0WaMQP4LfBSKRjpPO/9kBTqZs65q4HXSblIN3jvT5JCqaxZs4aDDjqIkZERKQrA0qVLpYioRwq0uM+QvpwPARdKIcNHgY1SKNKJzrk3SaFUZsyYwbHHHivFXlSr1aSIKGtBlafngcD7pJCCs733L0ihbue9vxe4RMopuNA5t4MUSuW1r32tFNlCbEmzFVS5nD3A14FeKRjp58AVUsi86C+BlVIo0nzgc1IolZNOOokpU6ZIMTXZCqrsvcChUkjBh3zuL+ltzHv/DPBZKafgXc65HP/+25g6dSonnHCCFNtCzBTNUlDl6TlAno3g297726SQ2cbFwANSKFIP8DXnXOojqAk1e5gbI0tBlX0VmCWFIq0DzpZCZlve+w3Ax6ScgoOAP5VCKRx//PFsv/32UmwLoVM0eUGVp+cpwOlSSMGF3vuHpJCZmPf+e8BPpZyCzzjnBqSQtv7+fk4++WQppiJpQZXLOR34Gymk4HHgfClkRB8CUn9/n0n9iCq7kMPckCmatKDKPgEMSiEFf+G9XyOFTGPe+/8ErpRyCk53zr1KCmk74ogjmDZtmhSLlqygytNzT+p75NTuB74phUxlnwCq3XYT5yvOuX4ppKmvr4+jjjpKim2j2SmarKDKvgxsJ4UUfMJ7n2OD6gre+/uBS6Wcgr0pcMLomGOOkSLRXIrLfMrT81XA9VJIwTBwkF331OWc25X6kclUKRvpOWCh9/4pKahl+fLlQVMUqt+n2+oTtB/4ihRSco6VU5/3/lHynNzbgcx3GA0MDDA4OCjFoqgXVHl6vp/64UtqP/XeXyeFTLDzgVVSSMFbnXMHSSFNoYe5Vb+LqhZUuZzzgE9KISUflwImnPf+aeACKaegh8yXXUILWpVqQZV9nvR3DAH8yHt/ixQy0S4Ecnw/PCrnI2mHHXYYvb1hdxxWmaJqBVWenocAZ0khBZuBc6SQiTd6bTnXDSBfdM5Nl0Iapk+fzpIlS6RYMLWCKvtrQLXxk7jKe/8rKWTU/C2wXAopeCkZ76U++uijpcikpCmqUlDl6XkGcLgUUuDJ81SMGeW9Xw98Ucop+bBzbhcppKGZVRaapVJQRf3kO1X+Pe/93VLIqPsG8IQUUjCd+gPkyS1atIjZs2dLsUk1mqLRBVWenu8Cfk8KKfDU1zMymY0uH/MlKafkrc65RVJIQ6opGl1QRTOp37uZww+993dJIZPMJcAKKaSgl0wnpmIvt0w2RaMKqjw9PwbMlUJKbHoW5L1fS76VEk9zzh0phWIdd9xxUiRIVEEVDQAflEJKrvXe/1IKmeQuBp6RQkqSn5iaNWsW+++/vxRraKIpGlxQ5en5aepf6nP4tBQw6XnvV5Pvrp8jnHN/IIVixVxumUxwQRW9nDw3JQBcN/ogsWkNX6X+FEoO5zvn+qRQjNjvobDtFA0qqPL0/Dzp17cdY9c9W4j3/jnyPOkCsBfwNikU48ADD1RfZSGooIoOA5pf3CXMrd77W6WQye4iYL0UUvIJ51yyVaf7+vpUDnPHT9GmC6o8PXN+H/wrKWDy894/DvyTlFOyK/AOKRRD4zB3vKYLquhI4EQppORe4BopZIr5EulXABzzcedcstUdtC+3NFVQ5emZ81rkBd77zVLIlOG9vwf4sZRTsgv1O9aSGBgYEG+Ar2Ls72iqoIqOBY6XQkqeBC6XQqa4nF9Bzk75OJrmYW7lgipPz5zfPS/23q+TQqYs7/1/ALdLOSUvAd4thUJpFbRWq1Vf1U+xoL8P3CiFlDwP7D665IZpcc65M4CrpJySJ4EFo7cdqlq3bh377LMPVbvVSKUJqlhOyPQI0KhLrZxt5fvA76SQknnUX2OpburUqey1115SrJJKBVV0EvWztzl48i3ZaRR47zeR7/Y/gI8652ZIoRAHHnigFKlELKjy9My1Sh/Ub+tL/Z5Ko+8yQP2wcxJzSHRGd/HixVKkErGgio4k3/SEfLeQGUWjt//lunEB4IPOOfXXimQpqPL0zLn27AOALUTdvnLuXAeAN0uhZi1cuJCZM2dKMVGuCbov9Zfv5nKJvcahfY2utJjjBcBjPuqcU+9C7POh0KCgytPzbPIsown1Sys53qhl0so5RfcC1J8X1ThRpL7XmMAewJlSSNG/eO9XSiHT8r4PPCaFFH1MCjSrXQr6USDpg7JbybnnNYl47zdSX6Izl0Odc6+UQs045JBDpIhowoIqHt7OBd4qhRTd4r0flkKmbXyDPG/oHqM6RWfOnMkee+whxRpKPUH/DNB9xLyxr0kB0z6898uBq6WcopOcc/HHpePEXm7ZpqCK03MaCW9InsBzwPekkGk7/yAFlH1ECjRDvaCK/hjYWQopumJ01XLTWa4nzwuXxpzhnJsvhaqKPVG0RUEVpyfA+6SAMru00oFG78/N+TxvP4q3/y1atIi+vvBzpKkm6HHAflJI0d3e+1zPEpr8cu9836l1+19/fz8HHHCAFJtUqoK+Xwooy/0PaDLy3t8H/EzKKZoPvFEKVaVSUMXD292B10khRRuBf5RCpu3l3gmrfUWL+R6aYoK+l3wLUQP82Hv/lBQybe8qYI0UUrTEOfcKKVRFdEEVp+c04O1SSFnuPaspYHRpklzLoYxR+ao2MDAQ/IJf7Qma+9LKk9hjZd3kW1JAmdoll4MPPliKTEi7oDlvTAD4rvc+561gpqyfAY9IIUX9wJ9IoSpCb1joUTy8PRgIP9gO820pYDrH6DO+V0o5ZW91CiUJLqgUaELSN0dN4GHgFilkOk7unfIgcIIUkixevDjoXI9WQacB/1sKKbvSVk3oPt77O4H7pJyy6OEzdepU9t57bym2Da2CngHsIIWUXSEFTMfK/W//eudc2GnYcRYtWiRFtqFV0Og9TJPuG92Tmu6U+zB3CgoLi+25555SZBsaBV0I6LyMorrce1DTQrz3vwXuknLKoofQggULpMg2NAoa/X88QO49qGk9ubeBfWPvLCpR0D7g/0ohZXeP7kFNd8t9VxFE3iW3YMECenqaq1xz6W2dTP3O/5xyLoFhWpT3/nfAr6WcsjOdc8FL+PT29rL77rtLsS3EFjT3pRWwgpr/kXtbmAmcKoUaafYwN6ag08n7WBnU10m9QwqZrpG7oBA5lHIW9HXA9lJI2bV2c4IZ5z+BR6WQslc754Kv+ecs6JukQAIl9pimRY3urK+RcsqmAG+QQpNp9lpoaEF3pH6CKKc1wE1SyHSdH0qBBIIPc3NN0DcAKosqNeHfvPfrpZDpOjdTXxM5p+NDnxOdPXs2M2ZUf6l3aEHt8Na0hNF3uOR+aL+HiBeCLVy4UIq8KKSg86kvq5mTJ/8/gmkfP5ICCQQPqWa+h4YU9A3kXRQMYNgWBjMN3Eh9J57TK5xzQW9GauZ7aEhBT5MCCfybFDDdy3v/BPArKZdAUBdSFnQH8h/eAtwgBUzXK7ETb7mCvpr6Qko5vUDeVcVNeypR0GOccztJoa3VarXKN81XS/2PoD1GpP+wyyumgp9S35nn1Ae8Rgptrbe3t/KLfZspaD/1CZqbHd4akfd+HfWS5hY0tKoe5jZT0GPJv+4QWEFNdSW2lZOdc1Ok0NZSFDRoTxHpCe997qUtTPsq8T10BvBKKbS1qtdCmylo7kfLAP6fFDBmzOjOfIWUS6Dp4aU9Qfel/lrB3OzsrWlWicXMm36IW7ugJ0qBRKygplkltpldnXNNLXq78847V7ppvpULuor8Syua9leioBDQkcHBQSlSqaD95F/3FuA27/1mKWTMVu4EnpdCCTRd0Dlz5kiRSgU9nPxLm0CZ7xKmzY0+fna7lEvgWOdcU3fZzZ07V4pUKmjTewYlpQ5VTPsrse3MBJpa2Fprgka/ei3ARuAXUsiYSZS4owiaHGYaBd0BOFTIpPBL732J7xGmM9wGbJJCCTRVUI1D3OPJ/3A22PQ0Ebz3q4HfSLkEljSzJKdGQY8Vfp6KvVrQxPr/UiCBXuBoKTRGo6BHCj9PpcSHazpLqZ38EVJgTOx30GnA4gY/T+UFwN5eZmK1fEFnzZpFf3/jKzONCnoo+VdPALjLez8ihYwRDFPmRNGSZq6Hzps3r+HPGxW08p5AWak9n+kgo1cB7pFyCTR15Ckd5rZiQe37p9FSamdfuTvSiaJGBT2swc9SKvWhms5TaluqfHI1tKAvA+RzwPrWA3dLIWMqKlXQyhM09BD38En+PLV7Rm92NkZDicWsof58aKUFDkInaInb+8AurxhF3vtV5H/B75glUgDCC3rAJH+emhXUaCu1TVU6kzt79uyGP5+ooA7Yf4I/z6HE/ZOms5XapioVNGSC1oBZE/x5DqX2dqZztXRBd9lll4Y/n6igpQ5vNwH3SSFjmlRqp7+rc0682Xa77bZj++0nX7BkooJWan4CD3jvN0ghY5pUaoJCxS41utTSShO01J7OdDDv/QrKLGYNFQva6Hto3wR/ZgU1neY3lFmZslJBL7vsMjZunPjy/9YF3QEYnCCXw/1SwJhA91OmoAf84T/FbdZbF3TvCVN5LJMCxgRaJgUSedmVb17YWxsaDn7sbevvoAsnTOWxTAoYE2iZFEhkO6Dam3on0SoF3Qw8JIWMCbRMCiQU1alWKehyu0neJLRUCiQU1alWKegyKWBMhOVAqWvsUZ2ygpqON/oSroelXCJRnRpf0LnUL7OUsEwKGBNpmRRIRK2gUX9RpGVSwJhIy6RAIoNLz11ceZW/rbVKQR+RAsZEKrWN9RFx88/4glZaoiGRJ6WAMZGekAIJBXdrfEF3nTSVXskPz3SHkttYcLfGF3Rg0lRaHnhKChkTqe0LGvyXRFppNymYDEoWNHj4tcIELfnBme5R8jxH8PAbK2gf0PgtLulYQU1yo0twviDlEoku6Hy2nKY5WUFNLqWmaHRBg/8CBaU+NNN9Sg2D+UvPXRw0AMd+qfHaf2k9LQWMUVJqbaJeAr9CjhW08fLWaa2SAsYoWS0FEgrq2FhBd2yYSqvkh2a6S8lhENSxVihoyQ/NdJeSwyCoY1ZQ001KbmtBHWuFgpbcq5nuYgUNUPJDM92l5DAI6thYQUutpABlPzTTXUoOg6iCBv2ykpIfmukuJbe1oI6NFbTU+0AB1koBY5SU3NaCjlLHCjqlYSote9TM5FJyWwvq2FhBgxc1iuRHl0Q0JocRKZBQUMfGCrpdw1Q6JfdopvuU3N6iChr0ywpK7tFM9ylZ0KAhaAU13aTk9hbUsR7AUX8cpoSSezTTfUpub8ETNOgXlZTco5nuU3J7C56gQb+opOQezXSfkttb0CDsodxaRADBrwY3JkDJ7S3oa2QPZcd+nxQwRlHbHS32EPiLSkp+YKb7lBwIQT0rXdCSH5jpPiUHQtAbvse+f5YqackPzHSfkgMhqGOlC1ryAzPdp+RAsAlqjKDkQAjq2FhBg9qtoOQHZrpPyYEQVdCgX9bgnLOSmlxKFjRoCI4VtNRbnyDwDgtjApQs6DopMJGxgj7XMJXWDClgjJKZUiChZ6XARMYKGvTLSkquh2S6S8ltLahjVlDTTUpua0Eda4WCljzsMN2l5LYW1LGxM6hBv6yk5F7NJHTKu598t3PubVJOk9/kz/zJ1+c9MMmPS25rQR2zgpqE3K7AwVJKU0+Pn9bgxyW3taCOtcIhbskPzXSXkttaUMdaoaAlvxeY7lJyWwvq2FhBn2qYSitoSXxjApScoCukwETGCvpYw1Rac6WAMUrmSYFE1teGhp+WQhMZK+ijDVNplfrQTPd5iRRIZLkUmMxYQVcA6xsFEyr1oZku4pxzwBwpl0jwABy/ol+pw1wrqMlhNuUeb4yeoBDxl0SygpocSm5nKhM0+C+JNN05t70UMiZS2xe01ASFsh+e6Q4lt7HgbrXCBIWyH57pDiW3seBujS/osslCGZT88Ex3KLmNPSgFJjO+oPdNmkpvdylgTKRS29h64GEpNJnxBb1/0lR6NSlgTKRBKZDIA7Wh4c1SaDLjC7qGctdCB6WAMZEGpUAiUUemW796MOovizAoBYwJ5ZzbDthFyiUS1SkrqOkGewBOCiUS1alWKeiOzjl77MykMigFEorqVKsUFOxEkUlnUAokFNWpVirooBQwJtCgFEjkBSJuUoBtC3ovMDJRMINBKWBMoEEpkMhvakPDXgo1snVB1wP3TBTMYC8pYEygUtvWsBSQbF1QUPhLAy2SAsY0a/RB7b2lXCL/JQUkVlDT6XYHSj3OGN2liQoa3fpA85xzO0shY5pUasfvUehSK01QKPdhms71cimQyNLa0PAqKSSZqKAriHjANFKpD9N0rlI7/ejpCRMXFMpN0VIfpulcpbYplQ5ZQU2nK7VNqXRosoLeMcmfp2aHuEaNc+4lQKkTj7dLgSomK+itk/x5ars753aSQsZUtFgKJLK0NjT8uBSqYrKCPglM9hLU1A6SAsZUlPXdpOOoDbjJCgqK/5EmWUGNllLbklp3WrGgpfZ6pvOU2pbUumMFNR1p9K60QSmXwGrgV1KoqkYF/TUQfSdEgD1tdQWjoNTh7e21oeFNUqiqRgXdDPyiwc9TccCBUsgYQakjMdUjz0YFBbhF+HkqpT5c0zlKTVDVzkgFvVn4eSpWUBOrxDa0kcwFvQ1YK2RSOFwKGDMZ59xcYE8pl8DPa0PDa6RQM6SCbgT+XcikMOic21UKGTOJo6RAIjdIgWZJBQW4UQokUupDNu2v1LZjBTWmghLbznMkeMikTwpQv+j6ODBfCior8SG3hYef3cBN961i2cr1PL12hGeeH2GTj1rdMZH8l7M3rH92CmUu092sef1zTJWCAtwE/JEUUra/c26W977EzRIt6fI7V3DZHStY+sx6KdoS9i5Q0KeX334A0C/lEkhypFm1oDeQv6A91M/m/qsU7HQ337+K8657hMdWb5SiXe/5VY+WmJ6Q4PsnVPsOCon+4xV0/WHuxT97grddtdTKWdGGdStL3KDwcG1o+F4pFKLqBF1OfQmH3A/AdnVBz/7xw1z1X89IMTPOyMa1+0uZBH4kBUJVnaAAV0uBBA5zzk2TQp3o0tufsnIG8H7TDCmTQLJutHpBpwJHS6FO88CKdXzuplIrn5omrSbhLbHNvnX4QepL6ed0gff+I1KoU5z67if29a5H7XlC08Y2u0ObmaAA10iBBE6UAsZ0qmYLWuIwdz/nXO6bJIxpCc0W9N+p39KUkwNOkEKdYP3IZiliukyzBd0I/EQKJdAVh7n3rmiPO4RMPs0WFMoc5nZFQR9fZTcjmC2FFPRHwPNSSNkuzrn9pFC7e2zVBiliukxIQdcC10qhBF4jBdrdyhfUH4YwbS6koADflgIJnCYF2p2nFR8ZMyWFFvQ64FkppOwVo2+rMqZrhBZ0A/B9KaTMAa+VQsZ0ktCCAvyLFEig4w9zjRkvpqA3U18KJacTnHPTpZAxnaLHh69lsxm4SgopmwqcJIWM6RQxExTsMNeYpGIL+gvgHimk7FTnXK8UMqYT9ABEHOYCfFMKKJsDHCeFjOkEsRMU4HLqN9Hn9CYpYEwneLGgEVP0SfLf+ve/nHMl1j41JiuNCQr5D3N3Al4lhYxpd1oFvR54RAops8Nc0/G2KGjEYe5m4FtSSNnrunVJTtM9tCYo1A9zgxseYAZwqhQypp1tU9CIKbqM+kuWcvpDKWBMO9OcoACXSAFlpzjnZkkhY9rVhAWNmKJXU1/cOpepwJlSyJh2pT1BNwF/K4WUvUUKGNOutAsK8PfAC1JI0eHOub2lkDHtaNLXD3rvca7ZV7cA8Azwz8DbpaCitwAfk0LtYLvekcc2bJ7yQSnXDrzzp+DzLpn61KO3MbIu/qXsM3ZacOWsnV/2cymX0ka36aGq7wdt1kXkLej/cc6d670fkYKtbnhgxdPAV6RcO9j74YE5zuUt6OMPXMfDd39NikmeBt5SGxrOeSQ4oYaHuBEni+6i/pqIXOYDr5ZCxlT0961QTkjzHXTMRVJAmZ0sMhpKnOiclFjQiCn6Q+AhKaToVOfcXClkjODq2tBwzu22IbGgETYBX5VCivqBN0shYwQXSIGcKhU0Yop+g/pZ3Vze4QJPPRsD/LQ2NHyrFMqpUkEjrAH+Rgop2osueZeoSeILUiC3ygWNmKJ/Td63ob1XChgzgV/VhoZ/LIVyWnru4uoFjbACuFQKKTrVObe7FDJmK1+UAiU0VdCIKfolINdNBL3Au6SQMeM8CFwhhXJaeu5ioMmCRngQuFIKKXq7c26KFDJm1AW1oeFcA6QpTRc0Yop+nnwrLswF3iiFjKH+FSz3oneVNV3QCL8GfiKFFNnJIlPFRbWh4ZwnMUVjh7cQWNCIKfpZKaDoFc65Q6SQ6WqryH9LalOCChrh59SX6MylIx7bMsl8pTY0vFIK5TR+ekJEQSOm6CelgKI3Oud2k0KmKz0LfFkKlRZc0Ah3ALkuCPcBH5BCpit9uTY0/KwUymnr6QmRBY2Yop+SAor+xDm3gxQyXWUlbfJQfFRBI9wJXCOFlMwE3imFTFe5oDY0HL8uiqKJpicoFDRiiv4F+a6Lvt/ehmZGPU39/vC2EF3QCL+kvo5uDrtiL1sydV+qDQ2vlkI5TTY9QamgEVP0U+Sboh+RAqbjraDFr3tuTaWgEe4CviOFlOznnGvpd4o67FnzxM6vDQ2vlUI5NZqeoFjQiCl6DrBRCik5RwqUtMPUXiliwj1I3sUDVKgVNMIDwNelkJJjnHPHSqFSZm+fapliA5xXGxpeL4VykqYnKBc0Yop+Gsj1xT3nnUxNmWMFTWWY+tsO2o5qQSG4pE+R74n2VzrnjpRCJew3fxr9PfY9NIE/rw0NB22YqVSZnpCgoBEuBJZLISUtOUVnTOnl6AUzpZhpzg21oeEbpFCrSlLQwCn6PPWbF3I4yTm3RAqVcNahtva2Ig/8uRTKrer0hEQFjXAp8FsppKQlp+hRtRkcutv2UsxU88+1oeFhKdTKkhU0cIpuAs6WQkpe45w7SAqVcM7vD0gRI1sPnCeFcmtmekLCgka4hnxvRmvJKXrAwHTee+RLpJhp7KLa0PCDUqjVJS1o4BSF+jOcm6WQgtNadVmUDx87nzMO2FmKmYk9AXxGCuXW7PSExAWF4JIOA38nhZR8TgqU8oXX7MZHjtvFbgBs3sdb7XGyUMkLGuE86g/Wpnaic+54KVTKe46YxzfPXMAuM+1puSp6+6bfDXxLyuUWMj0hU0EDp+gK8q28cL4UKOm4PWdyy/tezqdPfim1nW097kbmDCw5v9VuSojhAsvTtMC3AvZRP9zdRwoqeL33PtfzqVEefnYDN923imUr17Ni7Qgrnx9hU6Z/x2asumWHzzrHuVJOk/Ob95tx5OpfS7mtLXtmPU+uSbO4fOj0hHoBsvDeh5R0BPgz4EYpqGDIOXet9z7Hyakou+24HWcdOkeKFXfKLU9JkSSu+OPfkyLb+NS/Pso/3rlCimWX5RB3TOC0vgn4gRRSsA/wR1LImGbETE/IXNAIHwbWSSEFf2lrF+kZ2bh2RynTyWLLCQUKGjhFl1J/hWFqNeAdUshUs/bZpcdIGdNYz8aNuRYziPY56kVN7ZPOuVlSyDTmnFu8Yf1z+0q5TqUxPQF6brwxx/mXLQVO0ReA90ghBfNo8aVR2sSF4Js+K2i21HPttddKmSQCS3o9cJUUUvAB59ygFDITc86dBrTszR+paU1PGJ2ga9askXKt5APUXxuX0hTgC1LIbGv0JFuu1TE6Xt/IyAjXX389p59+upRVFzhFH3POnQNcLAUjvdE591Xv/a1S0GzhPcDLpFAnqw3pPYLaA/Cd7+RamlbNJdTfkpbal13A3RXdyjm3Ey36CF+76gG44447uPfee6Vsyxi92+ed1B/wTmkJ9sqIZnwKsGfkFL14HfSyyy5rlGs53vtfkuclOJ93zk2TQt3OObeQPGfZu8qLBf3BD37A6tW5lqZV80ngESkUaTfgQ1LI8FeA3YWl7MWCrlu3ju9+97uNsi3He7/Ge7+b994l/t+Q9P+l23nvX7/15zb3pUfZ5xZpi6dZLr/8cs4666yQp06M2Ubf5r5vbOwbuV7KaerftPl3Uqad/DepALQulhENnAAAAABJRU5ErkJggg=="></div>');
	div_to_render.append(addParentheses);
	addParentheses.popup({
	    content : "Adicionar parênteses",
	    delay: {
	      show: 750,
	      hide: 0
		}
	});

	addParentheses.on('click', function(mouse_event) {

		// verificar se já está ativado
		if (window.parentheses_activate) {
			return;
		}

		div_to_render.find('.usepointer').off('click');

		window.parentheses_activate = true;

		window.open_or_close = "open";

		div_to_render.find('.dropdown').addClass('disabled');

		div_to_render.find('.ghost_element').addClass('temp_class');
		div_to_render.find('.ghost_element').removeClass('ghost_element');

		var floatingObject = $('<div class="floating_parenthesis"> ( </div>');
		floatingObject.draggable().appendTo("body");
		floatingObject.css("position", "absolute");
		mouse_event.type = "mousedown.draggable";
		mouse_event.target = floatingObject[0];
		floatingObject.css("left", mouse_event.pageX + 10);
		floatingObject.css("top", mouse_event.pageY + 10);
		floatingObject.trigger(mouse_event);

		div_to_render.on('mousemove', function(evt) {
			var actual_target = null;
			if ($(evt.target).hasClass('single_element_expression')) {
				actual_target = $(evt.target);
			} else {
				actual_target = $(evt.target).closest('.single_element_expression');
			}

			if ($(evt.target).hasClass('temp_class') 
				|| actual_target.length < 1 
				|| actual_target.hasClass('add_parentheses')
				|| actual_target.hasClass('rendered_parentheses')
				|| $(evt.target).hasClass('expression_elements')) {
				return;
			}
			renderGhostParentheses(actual_target, command, function_obj, div_to_render, expression_array);
		});

		div_to_render.on('mouseleave', function() {
			/*window.open_parentheses.remove();
			window.close_parentheses.remove();*/
		});

		var floating;

		$('body').on('mouseup', function(evt) {

			if (window.open_or_close == "open") {

				window.open_or_close = "close";

				floatingObject.remove();

				var comando_que_esta = $(evt.target).closest('.command_container');
				var comando_certo = div_to_render.closest('.command_container');
				if (!comando_que_esta.is(comando_certo)) {

					window.parentheses_activate = false;
					div_to_render.find('.temp_class').addClass('ghost_element');
					div_to_render.find('.temp_class').removeClass('temp_class');
					div_to_render.off('mousemove');
					div_to_render.off('mouseleave');
					$('body').off('mouseup');
					window.open_parentheses.remove();
					window.close_parentheses.remove();
					window.inserir_open = -1;
					window.inserir_close = -1;
					window.open_or_close = null;
					renderExpression(command, function_obj, div_to_render, expression_array);

					return;
				}


				window.open_parentheses.addClass('parentheses_fixed');

				floating = $('<div class="floating_parenthesis"> ) </div>');
				floating.draggable().appendTo("body");
				floating.css("position", "absolute");
				floating.css("left", evt.pageX + 10);
				floating.css("top", evt.pageY + 10);

				$('body').on('mousemove', function(evts) {
					floating.css("left", evts.pageX + 10);
					floating.css("top", evts.pageY + 10);
				});				

			} else {

				floating.remove();

				window.open_parentheses.removeClass('parentheses_fixed');
				
				div_to_render.off('mousemove');
				div_to_render.off('mouseleave');
				$('body').off('mouseup');

				setTimeout(function(){
					window.parentheses_activate = false;
				}, 50);

				var comando_que_esta = $(evt.target).closest('.command_container');
				var comando_certo = div_to_render.closest('.command_container');
				var is_correct = false;
				if (comando_que_esta.is(comando_certo)) {
					is_correct = true;
				}

				if (is_correct) {
					expression_array.splice(window.inserir_open, 0, '(');
					expression_array.splice(window.inserir_close, 0, ')');
				}

				window.inserir_open = -1;
				window.inserir_close = -1;
				window.open_or_close = null;

				renderExpression(command, function_obj, div_to_render, expression_array);

			}

		});
	});
}

window.open_parentheses = $('<div class="parentheses_ghost">(</div>');
window.close_parentheses = $('<div class="parentheses_ghost">)</div>');
window.inserir_open = -1;
window.inserir_close = -1;
function renderGhostParentheses (actual_target, command, function_obj, div_to_render, expression_array) {

	/*window.open_parentheses.remove();
	window.close_parentheses.remove();*/

	var index_in_array = actual_target.data('index');

	if ((expression_array[index_in_array] == '(') || (expression_array[index_in_array] == ')')) {
		return;
	}

	if (window.open_or_close == "close") {
		if (index_in_array < window.inserir_open) {
			return;
		}
	}

	// Tratando a situação quando é na primeira posição:

	if (index_in_array == 0) {
		if (expression_array[index_in_array].type == "var_value") {

			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(actual_target);
				window.inserir_open = index_in_array;
			}

			/*if (expression_array.length == 1) {
				if (window.open_or_close == "close") {
					window.close_parentheses.insertAfter(actual_target);
					window.inserir_close = index_in_array + 2;
				}*/

			//} else {
				var count_opened = 0;
				var count_closed = 0;
				for (var i = 0; i < expression_array.length; i++) {
					if ((expression_array[i] == '(')) {
						count_opened ++;
					}
					if (expression_array[i] == ')') {
						count_closed ++;
					}
					if (count_opened != count_closed) {
					} else {
						if (count_opened > 0) {
							if (window.open_or_close == "close") {
								window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
								window.inserir_close = i + 2;
							}
							break;
						} else {
							if (expression_array[i].type == "var_value") {
								if (window.open_or_close == "close") {
									window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
									window.inserir_close = i + 2;
								}
								break;
							}
						}
					}

				}
			//}

		} else if (expression_array[index_in_array].type_op) {

			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(actual_target);
				window.inserir_open = index_in_array;
			}

			var count_opened = 0;
			var count_closed = 0;
			for (var i = 1; i < expression_array.length; i++) {
				// $('.slide-link[data-slide="0"]')
				if ((expression_array[i] == '(')) {
					count_opened ++;
				}
				if (expression_array[i] == ')') {
					count_closed ++;
				}
				if (count_opened != count_closed) {
				} else {
					if (count_opened > 0) {
						if (expression_array[i].type == "var_value") {
							window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
							window.inserir_close = i + 2;
						}

						break;
					} else {
						if (expression_array[i].type == "var_value") {
							if (expression_array[i].type == "var_value") {
								window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
								window.inserir_close = i + 2;
							}
							break;
						}
					}
				}

			}
		}
		return;
	}

	// Tratando quando não é no índice 0:
	if (expression_array[index_in_array].type == "var_value") {
		if (window.open_or_close == "open") {
			window.open_parentheses.insertBefore(actual_target);
			window.inserir_open = index_in_array;
		}
		if (window.open_or_close == "close") {
			window.close_parentheses.insertAfter(actual_target);
			window.inserir_close = index_in_array + 2;
		}
		return;
	}

	if (expression_array[index_in_array].type_op) {
		// buscar para a esquerda primeiro:
		if (expression_array[index_in_array - 1] == '(') {
			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(actual_target);
				window.inserir_open = index_in_array;
			}
		} else if (expression_array[index_in_array - 1] == ')') {
			// buscar a abertura
			var count_opened = 0;
			var count_closed = 0;
			for (var j = index_in_array - 1; j >= 0; j--) {
				if ((expression_array[j] == '(')) {
					count_opened ++;
				}
				if (expression_array[j] == ')') {
					count_closed ++;
				}

				if (count_opened != count_closed) {
				} else {
					if (count_closed > 0) {
						if (window.open_or_close == "open") {
							window.open_parentheses.insertBefore(div_to_render.find('.single_element_expression[data-index="'+j+'"]'));
							window.inserir_open = j;
						}
						break;
					}
				}

			}

		} else if (expression_array[index_in_array - 1].type == "var_value") {
			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(div_to_render.find('.single_element_expression[data-index="'+(index_in_array - 1)+'"]'));
				window.inserir_open = index_in_array - 1;
			}
		}

		// buscar para a direita agora:
		if (expression_array[index_in_array + 1] == '(') {
			// buscar o fechamento:

			var count_opened = 0;
			var count_closed = 0;
			for (var j = index_in_array + 1; j < expression_array.length; j ++) {
				if ((expression_array[j] == '(')) {
					count_opened ++;
				}
				if (expression_array[j] == ')') {
					count_closed ++;
				}

				if (count_opened != count_closed) {
				} else {
					if (count_opened > 0) {
						if (window.open_or_close == "close") {
							window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+j+'"]'));
							window.inserir_close = j + 2;
						}
						break;
					}
				}

			}


		} else if (expression_array[index_in_array + 1].type == "var_value") {
			if (window.open_or_close == "close") {
				window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+(index_in_array + 1)+'"]'));
				window.inserir_close = index_in_array + 3;
			}
		}
	}

}

function renderParenthesis (div_to_render, expression_content, command, function_obj, position, expression_array) {
	var ghost_parenthesis = $('<div class="single_element_expression parentheses_in_expression" data-index="'+position+'">'+expression_content+'</div>');
	div_to_render.append(ghost_parenthesis);
}

function renderStartAddOperator (div_to_render, types_included, expression_array, command, function_obj, position) {
	var menu_final = '<div class="ui dropdown disabled usepointer"><div class="text"> + </div><i class="dropdown icon"></i><div class="menu">';
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Aritméticos<div class="menu">';
			menu_final += getArithmeticOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getArithmeticOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Lógicos<div class="menu">';
			menu_final += getLogicOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getLogicOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Relacionais<div class="menu">';
			menu_final += getRelationalOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getRelationalOperators();
		}
	}
	menu_final += '</div></div>';

	menu_final = $(menu_final);
	var div_temp = $('<div class="single_element_expression ghost_element mouse_distance"></div>');
	div_temp.append(menu_final);
	var div_higher = $('<div class="higher_element"></div>');
	div_higher.append(div_temp);
	div_to_render.append(div_higher);
	menu_final.dropdown('set selected', Models.ARITHMETIC_TYPES.minus);

	div_temp.on('click', function() {
		if (!window.open_or_close) {
			var sera = position;

			if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
				console.log('p1');
				expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_arithmetic,Models.ARITHMETIC_TYPES.minus));
			} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
				console.log('p2');
				expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_logic,Models.LOGIC_COMPARISON.equals_to));
			} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
				console.log('p3');
				expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_conditional,Models.ARITHMETIC_COMPARISON.greater_than));
			}

			renderExpression(command, function_obj, div_to_render, expression_array);
		}
	});
}

function renderFinalAddElements (div_to_render, types_included, expression_array, command, function_obj, position, is_last = false) {

	var menu_final = '<div class="ui dropdown disabled usepointer"><div class="text"> + </div><i class="dropdown icon"></i><div class="menu">';
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Aritméticos<div class="menu">';
			menu_final += getArithmeticOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getArithmeticOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Lógicos<div class="menu">';
			menu_final += getLogicOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getLogicOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Relacionais<div class="menu">';
			menu_final += getRelationalOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getRelationalOperators();
		}
	}
	menu_final += '</div></div>';

	menu_final = $(menu_final);
	var div_temp = $('<div class="simple_add mouse_distance"></div>');
	var div_higher = $('<div class="higher_element"></div>');

	var button = $('<button class="ui button green add_expression"><i class="plus circle inverted icon"></i></button>');
	div_temp.append(button);

	if (!is_last) {
		div_higher.append(div_temp);
		div_to_render.append(div_higher);
		//div_temp.append(menu_final);
		div_temp.css('opacity', '0', '!important');
	} else {
		div_temp.removeClass('mouse_distance');
		div_temp.css('opacity', '1', '!important');
		//div_temp.append(menu_final);
		div_to_render.append(div_temp);
	}
	

	menu_final.dropdown('set selected', Models.ARITHMETIC_TYPES.plus);

	div_temp.on('click', function() {
		var sera = position;

		if (expression_array[sera] == ')' && expression_array[sera - 1] == '(') {
			expression_array.splice(sera, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
			renderExpression(command, function_obj, div_to_render, expression_array);
			return;
		}

		if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_arithmetic,Models.ARITHMETIC_TYPES.plus));
			expression_array.splice(sera + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_logic,Models.LOGIC_COMPARISON.equals_to));
			expression_array.splice(sera + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_conditional,Models.ARITHMETIC_COMPARISON.greater_than));
			expression_array.splice(sera + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
		}

		renderExpression(command, function_obj, div_to_render, expression_array);
	});
}

function renderOperatorMenu (command, function_obj, div_to_render, expression_element, types_included, position, expression_array) {

	var menu_final = '<div class="ui dropdown"><div class="text"> + </div><i class="dropdown icon"></i><div class="menu">';
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Aritméticos<div class="menu">';
			menu_final += getArithmeticOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getArithmeticOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Lógicos<div class="menu">';
			menu_final += getLogicOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getLogicOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Relacionais<div class="menu">';
			menu_final += getRelationalOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getRelationalOperators();
		}
	}
	menu_final += '</div></div>';

	menu_final = $(menu_final);
	var div_temp = $('<div class="single_element_expression not_allowed" data-index="'+position+'"></div>');
	div_temp.append(menu_final);
	div_to_render.append(div_temp);
	menu_final.dropdown({
		onChange: function(value, text, $selectedItem) {
      		expression_element.item = $selectedItem.data('value');
      		expression_element.type_op = $selectedItem.data('type');
    	}
	});

	menu_final.dropdown('set selected', expression_element.item);
}

function getArithmeticOperators () {
	var arithmetic_operators;
	arithmetic_operators = '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.plus+'">+</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.minus+'">-</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.multiplication+'">*</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.division+'">/</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.module+'">%</div>';
	return arithmetic_operators;
}

function getLogicOperators () {
	var logic_operators;
	logic_operators = '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.equals_to+'">==</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.not_equals_to+'">!=</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.and+'">'+LocalizedStrings.getUI('logic_operator_and')+'</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.or+'">'+LocalizedStrings.getUI('logic_operator_or')+'</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.not+'">'+LocalizedStrings.getUI('logic_operator_not')+'</div>';
	return logic_operators;
}

function getRelationalOperators () {
	var relational_operators;
	relational_operators = '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.greater_than+'">></div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.less_than+'"><</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.equals_to+'">==</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.not_equals_to+'">!=</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.greater_than_or_equals_to+'">>=</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.less_than_or_equals_to+'"><=</div>';
	return relational_operators;
}