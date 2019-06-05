import { RuntimeError } from './runtimeError';
import { SemanticError } from './semanticError';
import * as  LocalizedStringsService from './../../services/localizedStringsService';

const LocalizedStrings = LocalizedStringsService.getInstance();

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
  },        // TODO: better urgent error message
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
  unknown_command_full: (sourceInfo)=> {
    if(sourceInfo) {
      const context = [sourceInfo.line];
      return new RuntimeError(LocalizedStrings.getError("unknown_command_full", context));
    } else {
      return ProcessorErrorFactory.unknown_command();
    }
    
  },
  unknown_command: ()=> {
    return new RuntimeError(LocalizedStrings.getError("unknown_command"));
  },
  incompatible_types_full: (type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [LocalizedStrings.translateType(type, dim), sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("incompatible_types_full", context));
    } else {
      return ProcessorErrorFactory.incompatible_types(type, dim);
    }
  },
  incompatible_types: (type, dim) => {
    const context = [LocalizedStrings.translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("incompatible_types", context));
  },
  incompatible_types_array_full: (exp, type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [exp, LocalizedStrings.translateType(type, dim), sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("incompatible_types_array_full", context));
    } else {
      return ProcessorErrorFactory.incompatible_types_array(exp, type, dim);
    }
  },
  incompatible_types_array: (exp, type, dim) => {
    const context = [exp, LocalizedStrings.translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("incompatible_types_array", context));
  },
  loop_condition_type_full: (exp, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, sourceInfo.column, exp];
      return new SemanticError(LocalizedStrings.getError("loop_condition_type_full", context));
    } else {
      return ProcessorErrorFactory.loop_condition_type(exp);
    }
  },
  loop_condition_type: (exp) => {
    const context = [exp];
    return new SemanticError(LocalizedStrings.getError("loop_condition_type", context));
  },
  endless_loop_full: (sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line];
      return new SemanticError(LocalizedStrings.getError("endless_loop_full", context));
    } else {
      return ProcessorErrorFactory.endless_loop();
    }
  },
  endless_loop: () => {
    return new SemanticError(LocalizedStrings.getError("endless_loop"));
  },
  for_condition_type_full: (exp, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, sourceInfo.column, exp];
      return new SemanticError(LocalizedStrings.getError("for_condition_type_full", context));
    } else {
      return ProcessorErrorFactory.for_condition_type(exp);
    }
  },
  for_condition_type: (exp) => {
    const context = [exp];
    return new SemanticError(LocalizedStrings.getError("for_condition_type", context));
  },
  if_condition_type_full: (exp, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, sourceInfo.column, exp];
      return new SemanticError(LocalizedStrings.getError("if_condition_type_full", context));
    } else {
      return ProcessorErrorFactory.if_condition_type(exp);
    }
  },
  if_condition_type: (exp) => {
    const context = [exp];
    return new SemanticError(LocalizedStrings.getError("if_condition_type", context));
  },
  invalid_global_var: () => {
    return new RuntimeError(LocalizedStrings.getError("invalid_global_var"))
  },
  not_implemented: (id) => {
    const context  = [id]
    return new RuntimeError(LocalizedStrings.getError("not_implemented", context))
  },
  invalid_case_type_full: (exp, type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [exp, LocalizedStrings.translateType(type, dim), sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("invalid_case_type_full", context));
    } else {
      return ProcessorErrorFactory.invalid_case_type(exp, type, dim);
    }
  },
  invalid_case_type: (exp, type, dim) => {
    const context = [exp, LocalizedStrings.translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("invalid_case_type", context));
  },
  void_in_expression_full: (id, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, sourceInfo.column, id];
      return new SemanticError(LocalizedStrings.getError("void_in_expression_full", context));
    } else {
      return ProcessorErrorFactory.void_in_expression(id);
    }
  },
  void_in_expression: (id) => {
    const context = [id];
    return new SemanticError(LocalizedStrings.getError("void_in_expression", context));
  },
  invalid_array_access_full: (id, sourceInfo) => {
    if(sourceInfo) {
      const context = [id, sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("invalid_array_access_full", context));
    } else {
      return ProcessorErrorFactory.invalid_array_access(id);
    }
  },
  invalid_array_access: (id) => {
    const context = [id];
    return new SemanticError(LocalizedStrings.getError("invalid_array_access", context));
  },
  invalid_matrix_access_full: (id, sourceInfo) => {
    if(sourceInfo) {
      const context = [id, sourceInfo.line, sourceInfo.column];
      return new SemanticError(LocalizedStrings.getError("invalid_matrix_access_full", context));
    } else {
      return ProcessorErrorFactory.invalid_matrix_access(id);
    }
  },
  invalid_matrix_access: (id) => {
    const context = [id];
    return new SemanticError(LocalizedStrings.getError("invalid_matrix_access", context));
  },
  matrix_column_outbounds_full: (id, value, columns, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, value, id, columns];
      return new RuntimeError(LocalizedStrings.getError("matrix_column_outbounds_full", context));
    } else {
      return ProcessorErrorFactory.matrix_column_outbounds(id, value, columns);
    }
  },
  matrix_column_outbounds: (id, value, columns) => {
    const context = [value, id, columns];
    return new RuntimeError(LocalizedStrings.getError("matrix_column_outbounds", context));
  },
  matrix_line_outbounds_full: (id, value, lines, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, value, id, lines];
      return new RuntimeError(LocalizedStrings.getError("matrix_line_outbounds_full", context));
    } else {
      return ProcessorErrorFactory.matrix_line_outbounds(id, value, lines);
    }
  },
  matrix_line_outbounds: (id, value, lines) => {
    const context = [value, id, lines];
    return new RuntimeError(LocalizedStrings.getError("matrix_line_outbounds", context));
  },
  vector_line_outbounds_full: (id, value, lines, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, value, id, lines];
      return new RuntimeError(LocalizedStrings.getError("vector_line_outbounds_full", context));
    } else {
      return ProcessorErrorFactory.vector_line_outbounds(id, value, lines);
    }
  },
  vector_line_outbounds: (id, value, lines) => {
    const context = [value, id, lines];
    return new RuntimeError(LocalizedStrings.getError("vector_line_outbounds", context));
  },
  vector_not_matrix_full: (id, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, id];
      return new RuntimeError(LocalizedStrings.getError("vector_not_matrix_full", context));
    } else {
      return ProcessorErrorFactory.vector_not_matrix(id);
    }
  },
  vector_not_matrix: (id) => {
    const context = [id];
    return new RuntimeError(LocalizedStrings.getError("vector_not_matrix", context));
  },
  function_no_return: (id) => {
    const context = [id];
    return new SemanticError(LocalizedStrings.getError("function_no_return", context));
  },
  invalid_void_return_full: (id, type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, id, LocalizedStrings.translateType(type, dim)];
      return new SemanticError(LocalizedStrings.getError("invalid_void_return_full", context));
    } else {
      return ProcessorErrorFactory.invalid_void_return(id, type, dim);
    }
  },
  invalid_void_return: (id, type, dim) => {
    const context = [id, LocalizedStrings.translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("invalid_void_return_full", context));
  },
  invalid_return_type_full: (id, type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, id, LocalizedStrings.translateType(type, dim)];
      return new SemanticError(LocalizedStrings.getError("invalid_return_type_full", context));
    } else {
      return ProcessorErrorFactory.invalid_return_type(id, type, dim);
    }
  },
  invalid_return_type: (id, type, dim) => {
    const context = [id, LocalizedStrings.translateType(type, dim)];
    return new SemanticError(LocalizedStrings.getError("invalid_return_type", context));
  },
  invalid_parameters_size_full: (id, expected, actual, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, id, expected, actual];
      return new SemanticError(LocalizedStrings.getError("invalid_parameters_size_full", context));
    } else {
      return ProcessorErrorFactory.invalid_parameters_size(id, expected, actual);
    }
  },
  invalid_parameters_size: (id, expected, actual) => {
    const context = [id, expected, actual];
    return new SemanticError(LocalizedStrings.getError("invalid_parameters_size", context));
  },
  invalid_parameter_type_full: (id, exp, sourceInfo) => {
    if(sourceInfo) {
      const context = [exp, id, sourceInfo.line];
      return new SemanticError(LocalizedStrings.getError("invalid_parameter_type_full", context));
    } else {
      return ProcessorErrorFactory.invalid_parameter_type(id, exp);
    }
  },
  invalid_parameter_type: (id, exp) => {
    const context = [exp, id];
    return new SemanticError(LocalizedStrings.getError("invalid_parameter_type_full", context));
  },
  invalid_ref_full: (id, exp, sourceInfo) => {
    if(sourceInfo) {
      const context = [exp, id , sourceInfo.line];
      return new SemanticError(LocalizedStrings.getError("invalid_ref_full", context));
    } else {
      return ProcessorErrorFactory.invalid_ref(id, exp);
    }
  },
  invalid_ref: (id, exp) => {
    const context = [exp, id];
    return new SemanticError(LocalizedStrings.getError("invalid_ref", context));
  },
  unexpected_break_command_full: (sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line];
      return new RuntimeError(LocalizedStrings.getError("unexpected_break_command_full", context));
    } else {
      return ProcessorErrorFactory.unexpected_break_command();
    }
  },
  unexpected_break_command: () => {
    return new RuntimeError(LocalizedStrings.getError("unexpected_break_command"));
  },
  invalid_array_literal_type_full: (exp, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, exp];
      return new RuntimeError(LocalizedStrings.getError("invalid_array_literal_type_full", context));
    } else {
      return ProcessorErrorFactory.invalid_array_literal_type(exp);
    }
  },
  invalid_array_literal_type: (exp) => {
    const context = [exp];
    return new RuntimeError(LocalizedStrings.getError("invalid_array_literal_type", context));
  },
  invalid_array_literal_line_full: (expected, actual, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, expected, actual];
      return new RuntimeError(LocalizedStrings.getError("invalid_array_literal_line_full", context));
    } else {
      return ProcessorErrorFactory.invalid_array_literal_type(expected, actual);
    }
  },
  invalid_array_literal_line: (expected, actual) => {
    const context = [expected, actual];
    return new RuntimeError(LocalizedStrings.getError("invalid_array_literal_line", context));
  },
  invalid_array_literal_column_full: (expected, actual, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, expected, actual];
      return new RuntimeError(LocalizedStrings.getError("invalid_array_literal_column_full", context));
    } else {
      return ProcessorErrorFactory.invalid_array_literal_column(expected, actual);
    }
  },
  invalid_array_literal_column: (expected, actual) => {
    const context = [expected, actual];
    return new RuntimeError(LocalizedStrings.getError("invalid_array_literal_column", context));
  },
  invalid_unary_op_full: (expString, opName, type, dim, sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, expString, LocalizedStrings.translateOp(opName), LocalizedStrings.translateType(type, dim)];
      return new RuntimeError(LocalizedStrings.getError("invalid_unary_op_full", context));
    } else {
      return ProcessorErrorFactory.invalid_unary_op(opName, type, dim);
    }
  },
  invalid_unary_op: (expString, opName, type, dim) => {
    const context = [expString, LocalizedStrings.translateOp(opName), LocalizedStrings.translateType(type, dim)];
    return new RuntimeError(LocalizedStrings.getError("invalid_unary_op", context));
  },
  invalid_infix_op_full: (expString, opName, typeLeft, dimLeft, typeRight, dimRight,  sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line, expString, LocalizedStrings.translateOp(opName), LocalizedStrings.translateType(typeLeft, dimLeft), LocalizedStrings.translateType(typeRight, dimRight)];
      return new RuntimeError(LocalizedStrings.getError("invalid_infix_op_full", context));
    } else {
      return ProcessorErrorFactory.invalid_infix_op(opName, typeLeft, dimLeft, typeRight, dimRight);
    }
  },
  invalid_infix_op: (expString, opName, typeLeft, dimLeft, typeRight, dimRight) => {
    const context = [expString, LocalizedStrings.translateOp(opName), LocalizedStrings.translateType(typeLeft, dimLeft), LocalizedStrings.translateType(typeRight, dimRight)];
    return new RuntimeError(LocalizedStrings.getError("invalid_infix_op", context));
  },
  array_dimension_not_positive_full: (sourceInfo) => {
    if(sourceInfo) {
      const context = [sourceInfo.line];
      return new SemanticError(LocalizedStrings.getError("array_dimension_not_positive_full", context));
    } else {
      return ProcessorErrorFactory.array_dimension_not_positive();
    }
  },
  array_dimension_not_positive: () => {
    return new SemanticError(LocalizedStrings.getError("array_dimension_not_positive"));
  },
  invalid_type_conversion: (value, type, dim) => {
    const context = [value, LocalizedStrings.translateType(type, dim)];
    return new RuntimeError(LocalizedStrings.getError("invalid_type_conversion", context));
  },
  invalid_read_type: (exp, type, dim, name) => {
    const context = [exp, LocalizedStrings.translateType(type, dim), name];
    return new RuntimeError(LocalizedStrings.getError("invalid_read_type", context))
  },
  invalid_read_type_array: (exp, typePos, dimPos, name, typeArray, dimArray) => {
    const context = [exp, LocalizedStrings.translateType(typePos, dimPos), name,LocalizedStrings.translateType(typeArray, dimArray)];
    return new RuntimeError(LocalizedStrings.getError("invalid_read_type_array", context))
  }
});