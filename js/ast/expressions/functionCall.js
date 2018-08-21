export class FunctionCall {

	constructor (id, actualParameters) {
		this.id = id;
		this.actualParameters = actualParameters;
	}

	get parametersSize () {
		return this.actualParameters.length;
	}
}