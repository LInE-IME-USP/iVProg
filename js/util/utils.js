import { LanguageService } from "./../services/languageService";
import { LocalizedStrings } from "./../services/localizedStringsService";
import { Operators } from "./../ast/operators";

/** 
 * source: https://pawelgrzybek.com/page-scroll-in-vanilla-javascript/ 
 * 
*/
export function scrollIt (destination, duration = 200, easing = 'linear', callback = null) {

  const easings = {
    linear(t) {
      return t;
    },
    easeInQuad(t) {
      return t * t;
    },
    easeOutQuad(t) {
      return t * (2 - t);
    },
    easeInOutQuad(t) {
      return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t;
    },
    easeInCubic(t) {
      return t * t * t;
    },
    easeOutCubic(t) {
      return (--t) * t * t + 1;
    },
    easeInOutCubic(t) {
      return t < 0.5 ? 4 * t * t * t : (t - 1) * (2 * t - 2) * (2 * t - 2) + 1;
    },
    easeInQuart(t) {
      return t * t * t * t;
    },
    easeOutQuart(t) {
      return 1 - (--t) * t * t * t;
    },
    easeInOutQuart(t) {
      return t < 0.5 ? 8 * t * t * t * t : 1 - 8 * (--t) * t * t * t;
    },
    easeInQuint(t) {
      return t * t * t * t * t;
    },
    easeOutQuint(t) {
      return 1 + (--t) * t * t * t * t;
    },
    easeInOutQuint(t) {
      return t < 0.5 ? 16 * t * t * t * t * t : 1 + 16 * (--t) * t * t * t * t;
    }
  };

  const start = window.pageYOffset;
  const startTime = 'now' in window.performance ? performance.now() : new Date().getTime();

  const documentHeight = Math.max(document.body.scrollHeight, document.body.offsetHeight, document.documentElement.clientHeight, document.documentElement.scrollHeight, document.documentElement.offsetHeight);
  const windowHeight = window.innerHeight || document.documentElement.clientHeight || document.getElementsByTagName('body')[0].clientHeight;
  const destinationOffset = typeof destination === 'number' ? destination : destination.offsetTop;
  const destinationOffsetToScroll = Math.round(documentHeight - destinationOffset < windowHeight ? documentHeight - windowHeight : destinationOffset);

  if ('requestAnimationFrame' in window === false) {
    window.scroll(0, destinationOffsetToScroll);
    if (callback) {
      callback();
    }
    return;
  }

  function scroll() {
    const now = 'now' in window.performance ? performance.now() : new Date().getTime();
    const time = Math.min(1, ((now - startTime) / duration));
    const timeFunction = easings[easing](time);
    window.scroll(0, Math.ceil((timeFunction * (destinationOffsetToScroll - start)) + start));

    if (window.pageYOffset === destinationOffsetToScroll) {
      if (callback) {
        callback();
      }
      return;
    }

    requestAnimationFrame(scroll);
  }

  scroll();
}

/**
 * 
 * source: https://stackoverflow.com/a/16270434
 */
export function isElementInViewport (el) {
  const rect = el.getBoundingClientRect();

  return rect.bottom > 0 &&
    rect.right > 0 &&
    rect.left < (window.innerWidth || document.documentElement.clientWidth) &&
    rect.top < (window.innerHeight || document.documentElement.clientHeight);
}
let cacheMainList = null;
let cacheOp = null;
export function isKeyword (text) {
  fillCache();
  for (let key = 0; key < cacheMainList.length; ++key) {
    const keyword = cacheMainList[key];
    if(keyword == text) {
      return true;
    }
  }
  // not in main list, check op
  for (let op = 0; op < cacheOp.length; op++) {
    const lOp = cacheOp[op];
    if(lOp == text) {
      return true;
    }
  }
  return false;
}

export function isValidIdentifier (identifier_str) {
  const validRegex = /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(identifier_str);
  if(!validRegex) {
    return false;
  }
	return !isKeyword(identifier_str);
}

function fillCache () {
  if(cacheMainList == null) {
    cacheMainList = [];
    const mainList = ["RK_PROGRAM","RK_REAL","RK_VOID","RK_BOOLEAN","RK_STRING",
      "RK_INTEGER","RK_CHARACTER","RK_SWITCH","RK_CASE","RK_DEFAULT","RK_CONST",
      "RK_FUNCTION","RK_RETURN","RK_FOR","RK_BREAK","RK_DO","RK_WHILE","RK_IF",
      "RK_ELSE","RK_FALSE","RK_TRUE"];
    const lexerClass = LanguageService.getCurrentLexer();
    const nullLexer = new lexerClass();
    for (let key = 0; key < mainList.length; ++key) {
      const word = mainList[key];
      const keyword = nullLexer.literalNames[lexerClass[word]];
      cacheMainList.push(keyword.substring(1, keyword.length-1));
    }
  }
  if(cacheOp == null) {
    cacheOp = []
    const logicOpList = [Operators.AND.value, Operators.OR.value, Operators.NOT.value];
    for (let op = 0; op < logicOpList.length; ++op) {
      const lOp = `logic_operator_${logicOpList[op]}`;
      cacheOp.push(LocalizedStrings.getUI(lOp))
    }
  }
}
