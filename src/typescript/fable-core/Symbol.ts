const types = new Map<string, FunctionConstructor>();

export function setType(fullName: string, cons: FunctionConstructor) {
  types.set(fullName, cons);
}

export function getType(fullName: string) {
  return types.get(fullName);
}

export default {
  reflection: Symbol("reflection")
}
