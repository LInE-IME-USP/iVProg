export class Expression {

  constructor () {
    this._sourceInfo = null;
  }

  set sourceInfo (sourceInfo) {
		this._sourceInfo = sourceInfo;
	}

	get sourceInfo () {
		return this._sourceInfo;
	}
}