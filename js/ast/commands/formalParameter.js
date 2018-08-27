export class FormalParameter {

  constructor (type, id, dimensions, byRef = false) {
    this.type = type;
    this.id = id;
    this.dimensions = dimensions;
    this.byRef = byRef;
  }
}