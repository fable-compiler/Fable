import Long, { op_Division as Long_op_Division, toNumber as Long_toNumber } from "./Long.js";

export function create(h: number = 0, m: number = 0, s: number = 0, ms: number = 0) {
  if (h < 0 || m < 0 || s < 0 || ms < 0)
    throw new Error("The parameters describe an unrepresentable TimeOnly.");

  if (arguments.length === 1)
    // ticks
    return fromTicks(arguments[0]);
  else
    return h * 3600000 + m * 60000 + s * 1000 + ms;
}

export function fromTicks(ticks: Long) {
  return Long_toNumber(Long_op_Division(ticks, 10000));
}

export function fromTimeSpan(timeSpan: number) {
  if (timeSpan < 0 || timeSpan > 86399999)
    throw new Error("The TimeSpan describes an unrepresentable TimeOnly.");

  return timeSpan;
}

export function maxValue() {
  // This is "23:59:59.999"
  return 86399999;
}

export function add(t: number, ts: number) {
  const t2 = (t + ts) % 86400000;
  return t2 < 0 ? 86400000 + t2 : t2;
}

export function addHours(t: number, h: number) {
  return add(t, h * 3600000);
}

export function addMinutes(t: number, m: number) {
  return add(t, m * 60000);
}

export function isBetween(t: number, start: number, end: number) {
  if (start === end)
    return false;
  if (t === start)
    return true;

  if (start < end)
    return t > start && t < end;
  else
    return t > start || t < end;
}
