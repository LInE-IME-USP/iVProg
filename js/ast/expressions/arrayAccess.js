import { Expression } from './expression';

export class ArrayAccess extends Expression {
	
	constructor (id, line, column) {
		super();
		this.id = id;
		this.line = line;
		this.column = column;
	}

	toString () {
		const strLine = this.line.toString();
		let strColumn = null;
		if(this.column) {
			strColumn = this.column.toString();
		}
		if(strColumn) {
			return `${this.id}[${strLine}][${strColumn}]`;
		} else {
			return `${this.id}[${strLine}]`;
		}
	}
}