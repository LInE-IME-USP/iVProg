export class IVProgProcessor {

  constructor(ast, store) {
    this.ast = ast;
    this.store = store;
  }

  interpretAST () {
    this.initGlobal();
  }
}