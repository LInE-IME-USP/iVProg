export class CommandBlock {
	
	constructor(variables, commands) {
		this.variables = variables;
		this.commands = commands;
		this._sourceInfo = null;
	}

	set sourceInfo (sourceInfo) {
		this._sourceInfo = sourceInfo;
	}

	get sourceInfo () {
		return this._sourceInfo;
	}
}