export class SourceInfo {

  static createSourceInfo (token) {
    return new SourceInfo(token.line, token.column, token.text.length);
  }

  static createSourceInfoFromList (tokenA, tokenB) {
    const line = tokenA.line;
    const column = tokenA.column;
    // copied from https://github.com/UNIVALI-LITE/Portugol-Studio/blob/master/core/src/main/java/br/univali/portugol/nucleo/analise/sintatica/Portugol.g
    // No idea why...
    const size = tokenB.tokenIndex + 1 - tokenA.tokenIndex
    return new SourceInfo(line, column, size);
  }

  constructor (line, column, size) {
    this.line = line;
    this.column = column;
    this.size = size;
  }

}