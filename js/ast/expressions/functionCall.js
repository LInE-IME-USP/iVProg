import { Expression } from './expression';

export class FunctionCall extends Expression {

	constructor (id, actualParameters) {
		super();
		this.id = id;
		this.actualParameters = actualParameters;
	}

	get parametersSize () {
		return this.actualParameters.length;
	}
}