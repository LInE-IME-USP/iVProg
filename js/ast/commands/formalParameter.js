export class FormalParameter {

  constructor (type, id, byRef = false) {
    this.type = type;
    this.id = id;
    this.byRef = byRef;
    this._sourceInfo = null;
  }

  set sourceInfo (sourceInfo) {
		this._sourceInfo = sourceInfo;
	}

	get sourceInfo () {
		return this._sourceInfo;
	}
}