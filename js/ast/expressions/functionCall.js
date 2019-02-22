import { Expression } from './expression';
import { LanguageDefinedFunction } from '../../processor/definedFunctions';

export class FunctionCall extends Expression {

	constructor (id, actualParameters) {
		super();
		this.id = id;
		this.actualParameters = actualParameters;
	}

	get isMainCall () {
		return this.id === null;
	}

	get parametersSize () {
		return this.actualParameters.length;
	}

	toString () {
		let name = null;
		if(this.isMainCall) {
			name = LanguageDefinedFunction.getMainFunctionName();
		} else {
			name = LanguageDefinedFunction.getLocalName(this.id);
		}
		let params = null;
		if(this.actualParameters.length == 0) {
			params = "()";
		} else {
			const strParams = this.actualParameters.map(v => v.toString());
			params = "(" + strParams.join(",") + ")";
		}
		return name + params;
	}
}