import { Expression } from './expression';

export class ArrayAccess extends Expression {
	
	constructor (id, line, column) {
		super();
		this.id = id;
		this.line = line;
		this.column = column;
	}
}