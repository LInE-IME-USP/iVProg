export const Types = Object.freeze({
  INTEGER: "int",
  REAL: "real",
  STRING: "string",
  BOOLEAN: "bool",
  VOID: "void",
  ARRAY: 'array',
  UNDEFINED: 'undefined',
  ALL: 'all'
});

export function toInt (str) {
  if(str.match('^0b|^0B')) {
    return parseInt(str.substring(2), 2);
  } else if (str.match('^0x|^0X')) {
    return parseInt(str.substring(2), 16);
  } else {
    return parseInt(str);
  }
}

export function toString (str) {
  let value = str.replace("\\b", "\b");
  value = value.replace("\\t", "\t");
  value = value.replace("\\n", "\n");
  value = value.replace("\\r", "\r");
  value = value.replace("\\\"", "\"");
  value = value.replace("\\\'", "\'");
  value = value.replace("\\\\", "\\");
  return value;
}

export function toBool (str) {
  return true;
}