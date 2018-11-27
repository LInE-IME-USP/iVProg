import { RuntimeError } from './runtimeError';
import { SemanticError } from './semanticError';
import { LocalizedStrings } from './../../services/localizedStringsService';

function translateType (type, dim) {
  switch (dim) {
    case 0:
      return LocalizedStrings.getUI(type);
    default:
      const transType = LocalizedStrings.getUI(type);
      if(dim === 1)
        return LocalizedStrings.getUI("vector_string", [transType])
      else
        return LocalizedStrings.getUI("matrix_string", [transType])
  }
}

export const ProcessorErrorFactory  = Object.freeze({
  symbol_not_found_full: (id, sourceInfo) => {
    if(sourceInfo) {
      const context = [id, sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("symbol_not_found_full", context));
    } else {
      return ProcessorErrorFactory.symbol_not_found(id);
    }
  },
  symbol_not_found: (id) => {
    const context = [id];
    return new SemanticError(LocalizedStrings.getError("symbol_not_found", context));
  },
  function_missing_full: (id, sourceInfo) => {
    if(sourceInfo) {
      const context = [id, sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("function_missing_full", context));
    } else {
      return ProcessorErrorFactory.function_missing(id);
    }
  },
  function_missing: (id) => {
    const context = [id];
    return new SemanticError(LocalizedStrings.getError("function_missing", context));
  },
  main_missing: () => {
    return new SemanticError(LocalizedStrings.getError("main_missing"));
  },
  array_dimension_not_int_full: (sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line];
      return new SemanticError(LocalizedStrings.getError("array_dimension_not_int_full", context));
    } else {
      return ProcessorErrorFactory.array_dimension_not_int();
    }
  },
  array_dimension_not_int: () => {
    return new SemanticError(LocalizedStrings.getError("array_dimension_not_int"));
  },
  unknown_command: (id)=> {
    const context = [id];
    return new SemanticError(LocalizedStrings.getError("unknown_command", context));
  },
  incompatible_types_full: (type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [translateType(type, dim), sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("incompatible_types_full", context));
    } else {
      return ProcessorErrorFactory.incompatible_types(type, dim);
    }
  },
  incompatible_types: (type, dim) => {
    const context = [translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("incompatible_types", context));
  },
  incompatible_types_array_full: (exp, type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [exp, translateType(type, dim), sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("incompatible_types_array_full", context));
    } else {
      return ProcessorErrorFactory.incompatible_types_array(exp, type, dim);
    }
  },
  incompatible_types_array: (exp, type, dim) => {
    const context = [exp, translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("incompatible_types_array", context));
  },
  loop_condition_type_full: (sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("loop_condition_type_full", context));
    } else {
      return ProcessorErrorFactory.loop_condition_type();
    }
  },
  loop_condition_type: () => {
    return new SemanticError(LocalizedStrings.getError("loop_condition_type"));
  },
  for_condition_type_full: (sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("for_condition_type_full", context));
    } else {
      return ProcessorErrorFactory.for_condition_type();
    }
  },
  for_condition_type: () => {
    return new SemanticError(LocalizedStrings.getError("for_condition_type"));
  },
  if_condition_type_full: (sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("if_condition_type_full", context));
    } else {
      return ProcessorErrorFactory.if_condition_type();
    }
  },
  if_condition_type: () => {
    return new SemanticError(LocalizedStrings.getError("if_condition_type"));
  },
  invalid_case_type_full: (exp, type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [exp, translateType(type, dim), sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("invalid_case_type_full", context));
    } else {
      return ProcessorErrorFactory.invalid_case_type(exp, type, dim);
    }
  },
  invalid_case_type: (exp, type, dim) => {
    const context = [exp, translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("invalid_case_type", context));
  }
});