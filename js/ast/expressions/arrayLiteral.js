import { Literal } from './literal';
import { Types } from './../types';

export class ArrayLiteral extends Literal {
  
  constructor(value) {
    super(Types.ARRAY);
    this.value = value;
  }

  get subtype () {
    let element = this.value[0];
    if (element instanceof ArrayLiteral) {
      return element.value[0].type;
    } else {
      return element.type;
    }
  }

  get lines () {
    return this.value.length;
  }

  get columns () {
    let element = this.value[0];
    if (!(element instanceof ArrayLiteral)){
      return null;
    } else {
      return element.value[0].value.length;
    }
  }

  get isVector () {
    return this.columns === null;
  }

  get isValid () {
    return true;//this.validateType() && this.validateSize();
  }

  validateType () {
    // let valid = true;
    // if(this.columns !== null) {
    //   const len = this.columns;
    //   const len2 = this.lines;
    //   for (let i = len - 1; i >= 0; i--) {
    //     for (let j = len2 - 1; j >= 0; j--) {
    //       if(this.value[i].value[j].type !== this.subtype) {
    //         valid = false;
    //         break;
    //       }
    //     }
    //   }
    // } else {
    //   const len = this.lines;
    //   for (var i = len - 1; i >= 0; i--) {
    //     if(this.value[i].type !== this.subtype) {
    //       valid = false;
    //       break;
    //     }
    //   }
    // }
    return true;//valid;
  }

  validateSize () {
    let valid = true;
    if(this.columns !== null) {
      const equalityTest = data.value.map(v => v.length)
      .reduce((old, next) => {
        if (old === null) {
          return next;
        } else if (old === next) {
          return old
        } else {
          return -1;
        }
      }, null);
      valid = equalityTest !== -1;
    }
    return valid;
  }
}