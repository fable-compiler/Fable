import { createMutable as createMutableMap } from "./Map";
import { createMutable as createMutableSet } from "./Set";
import { declare } from "./Types";

export const Dictionary = declare(function Dictionary(source, comparer) {
  this.__mutableMap = createMutableMap(source, comparer);
});
Object.defineProperty(Dictionary.prototype, "size", { get: function() {
  return this.__mutableMap.size;
}});
Dictionary.prototype.clear = function() { return this.__mutableMap.clear(); };
Dictionary.prototype.delete = function(k) { return this.__mutableMap.delete(k); };
Dictionary.prototype.entries = function() { return this.__mutableMap.entries(); };
Dictionary.prototype.get = function(k) { return this.__mutableMap.get(k); };
Dictionary.prototype.has = function(k) { return this.__mutableMap.has(k); };
Dictionary.prototype.keys = function() { return this.__mutableMap.keys(); };
Dictionary.prototype.set = function(k, v) { return this.__mutableMap.set(k, v); };
Dictionary.prototype.values = function() { return this.__mutableMap.values(); };
Dictionary.prototype[Symbol.iterator] = function() { return this.__mutableMap[Symbol.iterator](); };

export const HashSet = declare(function HashSet(source, comparer) {
  this.__mutableSet = createMutableSet(source, comparer);
});
Object.defineProperty(HashSet.prototype, "size", { get: function() {
  return this.__mutableSet.size;
}});
HashSet.prototype.add = function(v) { return this.__mutableSet.add(v); };
HashSet.prototype.clear = function() { return this.__mutableSet.clear(); };
HashSet.prototype.delete = function(k) { return this.__mutableSet.delete(k); };
HashSet.prototype.has = function(k) { return this.__mutableSet.has(k); };
HashSet.prototype.values = function() { return this.__mutableSet.values(); };
HashSet.prototype[Symbol.iterator] = function() { return this.__mutableSet[Symbol.iterator](); };
