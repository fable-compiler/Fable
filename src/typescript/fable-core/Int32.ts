// TODO verify that this matches the behavior of .NET
export const parseRadix10 = /^ *([0-9]+) *$/;

// TODO verify that this matches the behavior of .NET
export const parseRadix16 = /^ *([0-9a-fA-F]+) *$/;

export function parseInt16(v: string): number {
  return parseInt(v, 16);
}
