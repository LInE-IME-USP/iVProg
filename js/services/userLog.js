let clicks = [];
let inRebuildMode = false;

export const ActionTypes = Object.freeze({
  // registerUserEvent payload:
  // (function_name, insert action)
  INSERT_FUNCTION:"INSERT_FUNCTION",
  // (function_name, remove action)
  REMOVE_FUNCTION:"REMOVE_FUNCTION",
  // (var_name,insert action, type, dim)
  INSERT_GLOBAL_VAR:"INSERT_GLOBAL_VAR",
  // (var name, remove action)
  REMOVE_GLOBAL_VAR:"REMOVE_GLOBAL_VAR",
  // (old function name, rename action, new function name)
  RENAME_FUNCTION:"RENAME_FUNCTION",
  // (old var name, rename action. new var name)
  RENAME_GLOBAL_VAR:"RENAME_GLOBAL_VAR",
  // (var name, set const action)
  SET_GLOBAL_CONST:"SET_GLOBAL_CONST",
  // (function name, change return action, type, dim)
  CHANGE_FUNCTION_RETURN:"CHANGE_FUNCTION_RETURN",
  // (var name, change type action, type, dim, rows?, columns?)
  CHANGE_GLOBAL_TYPE:"CHANGE_GLOBAL_TYPE",
  // (var name, change value action, value)
  CHANGE_GLOBAL_VALUE:"CHANGE_GLOBAL_VALUE",
  // (function name, insert param action, param varname, type, dim)
  INSERT_FUNCTION_PARAM:"INSERT_FUNCTION_PARAM",
  // (function name, remove param action, param varname)
  REMOVE_FUNCTION_PARAM:"REMOVE_FUNCTION_PARAM",
  // (function name, change param type action, type, dim)
  CHANGE_PARAM_TYPE:"CHANGE_PARAM_TYPE",
  // (function name, rename param action, old param name, new param name)
  RENAME_FUNCTION_PARAM:"RENAME_FUNCTION_PARAM",
  // (function name, insert var action, var name, type, dim)
  INSERT_FUNCTION_VAR:"INSERT_FUCNTION_VAR",
  // (function name, remove var action, var name)
  REMOVE_FUNCTION_VAR:"REMOVE_FUNCTION_VAR",
  // (function name, rename var action, old var name, new var name)
  RENAME_FUNCTION_VAR:"RENAME_FUNCTION_VAR",
  // (function name, change var type action, var name, type, dim, rows?, columns?)
  CHANGE_VAR_TYPE:"CHANGE_VAR_TYPE",
  // (function name, change var value action, var name, value)
  CHANGE_VAR_VALUE:"CHANGE_VAR_VALUE",
  // (function name, insert command action, command, path)
  INSERT_COMMAND:"INSERT_COMMAND",
  // (function name, remove command action, path)
  REMOVE_COMMAND:"REMOVE_COMMAND",
  // (function name, change command exp action, path, exp)
  CHANGE_COMMAND_EXP:"CHANGE_COMMAND_EXP",
  // (function name, change attrib exp action, path, exp)
  CHANGE_ATTRIB_EXP:"CHANGE_ATTRIB_EXP",
  // (function name, change attrib var action, path, var name, row_exp?, column_exp?)
  CHANGE_ATTRIB_VAR:"CHANGE_ATTRIB_VAR",
  // (function name, move command action, old path, new path)
  MOVE_COMMAND:"MOVE_COMMAND",
  // (function name, enter change name action, var name)
  ENTER_CHANGE_VAR_NAME:'ENTER_CHANGE_VAR_NAME',
  // (function name, enter change value action, var name)
  ENTER_CHANGE_VAR_VALUE:'ENTER_CHANGE_VAR_VALUE',
  // (var name, enter change name action)
  ENTER_CHANGE_GLOBAL_NAME:'ENTER_CHANGE_GLOBAL_NAME',
  // (var name, enter change value action)
  ENTER_CHANGE_GLOBAL_VALUE:'ENTER_CHANGE_GLOBAL_VALUE',
  // (function name, enter change name action, param name)
  ENTER_CHANGE_PARAM_NAME:'ENTER_CHANGE_PARAM_NAME',
});

export function registerClick (x, y, details) {
  if (inRebuildMode) {
    return;
  }
  clicks.push([x, y, Date.now(), details]);
}

export function registerUserEvent (command, action, ...params) {
  registerEvent('user_event', command, action, params);
}

export function registerSystemEvent (command, action, ...params) {
  registerEvent('system_event', command, action, params);
}

function registerEvent (tag, command, action, params) {
  if (inRebuildMode) {
    return;
  }
  const event = {context: command, action: action, params:params};
  clicks.push([tag, Date.now(), event]);
}

export function getLogs () {
  return clicks;
}

export function setRebuildMode (flag) {
  inRebuildMode = flag;
}

export function getLogsAsString () {
  return JSON.stringify(clicks);
}

export function parseLogs (logData) {
  clicks = JSON.parse(logData);
}
