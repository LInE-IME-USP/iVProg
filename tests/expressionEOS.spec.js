import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import {
	SyntaxError
} from './../js/ast/SyntaxError';

description('Expressions which ends with ID terminals:', () => {
	const input = 'test = i';
	const lexer  = Lexers['pt_br'];

	it('\'val = i\' should not result in SyntaxError', () => {
		const as = new IVProgParser(input, lexer);
        expect(as.parseExpressionOR).not.toThrow(SyntaxError);
	});

	if("'val = 2 + vector[1]' should not result in SyntaxError", () => {
		const as = new IVProgParser(input, lexer);
        expect(as.parseExpressionOR).not.toThrow(SyntaxError);
	});
});