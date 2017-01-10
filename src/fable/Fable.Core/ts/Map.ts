import List from "./ListClass"
import { ofArray as listOfArray } from "./ListClass"
import { IComparer } from "./Util"
import { IEquatable } from "./Util"
import { IComparable } from "./Util"
import { toString } from "./Util"
import { equals } from "./Util"
import { compare } from "./Util"
import GenericComparer from "./GenericComparer"
import FSymbol from "./Symbol"
import { map as seqMap } from "./Seq"
import { fold as seqFold } from "./Seq"
import { reduce as seqReduce } from "./Seq"
import { forAll as seqForAll } from "./Seq"
import { exists as seqExists } from "./Seq"
import { pick as seqPick } from "./Seq"
import { tryPick as seqTryPick } from "./Seq"
import { compareWith as seqCompareWith } from "./Seq"

// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies

export function groupBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
  const keys: K[] = [], iter = xs[Symbol.iterator]();
  let acc = create<K, T[]>(), cur = iter.next();
  while (!cur.done) {
    const k = f(cur.value), vs = tryFind(k, acc);
    if (vs == null) {
      keys.push(k);
      acc = add<K, T[]>(k, [cur.value], acc);
    }
    else {
      vs.push(cur.value);
    }
    cur = iter.next();
  }
  return keys.map(k => [k, acc.get(k)] as [K, T[]]);
}

export function countBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
  return groupBy(f, xs).map(kv => [kv[0], kv[1].length] as [K, number]);
}
// ----------------------------------------------

interface MapIterator {
  stack: List<MapTree>;
  started: boolean;
}

export class MapTree {
  public Case: "MapEmpty" | "MapOne" | "MapNode";
  public Fields: any[];

  constructor(caseName: "MapEmpty" | "MapOne" | "MapNode", fields: any[]) {
    this.Case = caseName;
    this.Fields = fields;
  }
}

function tree_sizeAux(acc: number, m: MapTree): number {
    return m.Case === "MapOne"
        ? acc + 1
        : m.Case === "MapNode"
            ? tree_sizeAux(tree_sizeAux(acc + 1, m.Fields[2]), m.Fields[3])
            : acc;
}

function tree_size(x: MapTree) {
    return tree_sizeAux(0, x);
}

function tree_empty() {
    return new MapTree("MapEmpty", []);
}

function tree_height(_arg1: MapTree) {
    return _arg1.Case === "MapOne" ? 1 : _arg1.Case === "MapNode" ? _arg1.Fields[4] : 0;
}

function tree_isEmpty(m: MapTree) {
    return m.Case === "MapEmpty" ? true : false;
}

function tree_mk(l: MapTree, k: any, v: any, r: MapTree) {
    var matchValue = [l, r];
    var $target1 = () => {
        var hl = tree_height(l);
        var hr = tree_height(r);
        var m = hl < hr ? hr : hl;
        return new MapTree("MapNode", [k, v, l, r, m + 1]);
    };
    if (matchValue[0].Case === "MapEmpty") {
        if (matchValue[1].Case === "MapEmpty") {
            return new MapTree("MapOne", [k, v]);
        } else {
            return $target1();
        }
    } else {
        return $target1();
    }
};

function tree_rebalance(t1: MapTree, k: any, v: any, t2: MapTree) {
    var t1h = tree_height(t1);
    var t2h = tree_height(t2);
    if (t2h > t1h + 2) {
        if (t2.Case === "MapNode") {
            if (tree_height(t2.Fields[2]) > t1h + 1) {
                if (t2.Fields[2].Case === "MapNode") {
                    return tree_mk(tree_mk(t1, k, v, t2.Fields[2].Fields[2]), t2.Fields[2].Fields[0], t2.Fields[2].Fields[1], tree_mk(t2.Fields[2].Fields[3], t2.Fields[0], t2.Fields[1], t2.Fields[3]));
                } else {
                    throw new Error("rebalance");
                }
            } else {
                return tree_mk(tree_mk(t1, k, v, t2.Fields[2]), t2.Fields[0], t2.Fields[1], t2.Fields[3]);
            }
        } else {
            throw new Error("rebalance");
        }
    } else {
        if (t1h > t2h + 2) {
            if (t1.Case === "MapNode") {
                if (tree_height(t1.Fields[3]) > t2h + 1) {
                    if (t1.Fields[3].Case === "MapNode") {
                        return tree_mk(tree_mk(t1.Fields[2], t1.Fields[0], t1.Fields[1], t1.Fields[3].Fields[2]), t1.Fields[3].Fields[0], t1.Fields[3].Fields[1], tree_mk(t1.Fields[3].Fields[3], k, v, t2));
                    } else {
                        throw new Error("rebalance");
                    }
                } else {
                    return tree_mk(t1.Fields[2], t1.Fields[0], t1.Fields[1], tree_mk(t1.Fields[3], k, v, t2));
                }
            } else {
                throw new Error("rebalance");
            }
        } else {
            return tree_mk(t1, k, v, t2);
        }
    }
}

function tree_add(comparer: IComparer<any>, k: any, v: any, m: MapTree): MapTree {
    if (m.Case === "MapOne") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return new MapTree("MapNode", [k, v, new MapTree("MapEmpty", []), m, 2]);
        }
        else if (c === 0) {
            return new MapTree("MapOne", [k, v]);
        }
        return new MapTree("MapNode", [k, v, m, new MapTree("MapEmpty", []), 2]);
    }
    else if (m.Case === "MapNode") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return tree_rebalance(tree_add(comparer, k, v, m.Fields[2]), m.Fields[0], m.Fields[1], m.Fields[3]);
        }
        else if (c === 0) {
            return new MapTree("MapNode", [k, v, m.Fields[2], m.Fields[3], m.Fields[4]]);
        }
        return tree_rebalance(m.Fields[2], m.Fields[0], m.Fields[1], tree_add(comparer, k, v, m.Fields[3]));
    }
    return new MapTree("MapOne", [k, v]);
}

function tree_find(comparer: IComparer<any>, k: any, m: MapTree): any {
    const res = tree_tryFind(comparer, k, m);
    if (res != null)
        return res;
    throw new Error("key not found");
}

function tree_tryFind(comparer: IComparer<any>, k: any, m: MapTree): any {
    if (m.Case === "MapOne") {
        var c = comparer.Compare(k, m.Fields[0]);
        return c === 0 ? m.Fields[1] : null;
    }
    else if (m.Case === "MapNode") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return tree_tryFind(comparer, k, m.Fields[2]);
        } else {
            if (c === 0) {
                return m.Fields[1];
            } else {
                return tree_tryFind(comparer, k, m.Fields[3]);
            }
        }
    }
    return null;
}

function tree_partition1(comparer: IComparer<any>, f: (k: any, v: any) => boolean, k: any, v: any, acc1: MapTree, acc2: MapTree): [MapTree, MapTree] {
    return f(k, v) ? [tree_add(comparer, k, v, acc1), acc2] : [acc1, tree_add(comparer, k, v, acc2)];
}

function tree_partitionAux(comparer: IComparer<any>, f: (k: any, v: any) => boolean, s: MapTree, acc_0: MapTree, acc_1: MapTree): [MapTree, MapTree] {
    const acc: [MapTree, MapTree] = [acc_0, acc_1];
    if (s.Case === "MapOne") {
        return tree_partition1(comparer, f, s.Fields[0], s.Fields[1], acc[0], acc[1]);
    }
    else if (s.Case === "MapNode") {
        const acc_2 = tree_partitionAux(comparer, f, s.Fields[3], acc[0], acc[1]);
        const acc_3 = tree_partition1(comparer, f, s.Fields[0], s.Fields[1], acc_2[0], acc_2[1]);
        return tree_partitionAux(comparer, f, s.Fields[2], acc_3[0], acc_3[1]);
    }
    return acc;
}

function tree_partition(comparer: IComparer<any>, f: (k: any, v: any) => boolean, s: MapTree) {
    return tree_partitionAux(comparer, f, s, tree_empty(), tree_empty());
}

function tree_filter1(comparer: IComparer<any>, f: (k: any, v: any) => boolean, k: any, v: any, acc: MapTree) {
    return f(k, v) ? tree_add(comparer, k, v, acc) : acc;
}

function tree_filterAux(comparer: IComparer<any>, f: (k: any, v: any) => boolean, s: MapTree, acc: MapTree): MapTree {
    return s.Case === "MapOne" ? tree_filter1(comparer, f, s.Fields[0], s.Fields[1], acc) : s.Case === "MapNode" ? tree_filterAux(comparer, f, s.Fields[3], tree_filter1(comparer, f, s.Fields[0], s.Fields[1], tree_filterAux(comparer, f, s.Fields[2], acc))) : acc;
}

function tree_filter(comparer: IComparer<any>, f: (k: any, v: any) => boolean, s: MapTree) {
    return tree_filterAux(comparer, f, s, tree_empty());
}

function tree_spliceOutSuccessor(m: MapTree): [any, any, MapTree] {
    if (m.Case === "MapOne") {
        return [m.Fields[0], m.Fields[1], new MapTree("MapEmpty", [])];
    }
    else if (m.Case === "MapNode") {
        if (m.Fields[2].Case === "MapEmpty") {
            return [m.Fields[0], m.Fields[1], m.Fields[3]];
        }
        else {
            const kvl = tree_spliceOutSuccessor(m.Fields[2]);
            return [kvl[0], kvl[1], tree_mk(kvl[2], m.Fields[0], m.Fields[1], m.Fields[3])];
        }
    }
    throw new Error("internal error: Map.spliceOutSuccessor");
}

function tree_remove(comparer: IComparer<any>, k: any, m: MapTree): MapTree {
    if (m.Case === "MapOne") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c === 0) {
            return new MapTree("MapEmpty", []);
        } else {
            return m;
        }
    }
    else if (m.Case === "MapNode") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return tree_rebalance(tree_remove(comparer, k, m.Fields[2]), m.Fields[0], m.Fields[1], m.Fields[3]);
        } else {
            if (c === 0) {
                var matchValue = [m.Fields[2], m.Fields[3]];
                if (matchValue[0].Case === "MapEmpty") {
                    return m.Fields[3];
                } else {
                    if (matchValue[1].Case === "MapEmpty") {
                        return m.Fields[2];
                    } else {
                        var patternInput = tree_spliceOutSuccessor(m.Fields[3]);
                        var sv = patternInput[1];
                        var sk = patternInput[0];
                        var r_ = patternInput[2];
                        return tree_mk(m.Fields[2], sk, sv, r_);
                    }
                }
            } else {
                return tree_rebalance(m.Fields[2], m.Fields[0], m.Fields[1], tree_remove(comparer, k, m.Fields[3]));
            }
        }
    }
    else {
        return tree_empty();
    }
}

function tree_mem(comparer: IComparer<any>, k: any, m: MapTree): boolean {
    if (m.Case === "MapOne") {
        return comparer.Compare(k, m.Fields[0]) === 0;
    }
    else if (m.Case === "MapNode") {
        var c = comparer.Compare(k, m.Fields[0]);
        if (c < 0) {
            return tree_mem(comparer, k, m.Fields[2]);
        } else {
            if (c === 0) {
                return true;
            } else {
                return tree_mem(comparer, k, m.Fields[3]);
            }
        }
    }
    else {
        return false;
    }
}

function tree_iter(f: (k: any, v: any) => void, m: MapTree): void {
    if (m.Case === "MapOne") {
        f(m.Fields[0], m.Fields[1]);
    }
    else if (m.Case === "MapNode") {
        tree_iter(f, m.Fields[2]);
        f(m.Fields[0], m.Fields[1]);
        tree_iter(f, m.Fields[3]);
    }
}

function tree_tryPick(f: (k: any, v: any) => any, m: MapTree): any {
    if (m.Case === "MapOne") {
        return f(m.Fields[0], m.Fields[1]);
    }
    else if (m.Case === "MapNode" ) {
        var matchValue = tree_tryPick(f, m.Fields[2]);
        if (matchValue == null) {
            var matchValue_1 = f(m.Fields[0], m.Fields[1]);
            if (matchValue_1 == null) {
                return tree_tryPick(f, m.Fields[3]);
            } else {
                var res = matchValue_1;
                return res;
            }
        } else {
            var res = matchValue;
            return res;
        }
    }
    else {
        return null;
    }
}

function tree_exists(f: (k: any, v: any) => boolean, m: MapTree): boolean {
    return m.Case === "MapOne" ? f(m.Fields[0], m.Fields[1]) : m.Case === "MapNode" ? (tree_exists(f, m.Fields[2]) ? true : f(m.Fields[0], m.Fields[1])) ? true : tree_exists(f, m.Fields[3]) : false;
}

function tree_forall(f: (k: any, v: any) => boolean, m: MapTree): boolean {
    return m.Case === "MapOne" ? f(m.Fields[0], m.Fields[1]) : m.Case === "MapNode" ? (tree_forall(f, m.Fields[2]) ? f(m.Fields[0], m.Fields[1]) : false) ? tree_forall(f, m.Fields[3]) : false : true;
}

// function tree_map(f: (v:any) => any, m: MapTree): MapTree {
//   return m.Case === "MapOne" ? new MapTree("MapOne", [m.Fields[0], f(m.Fields[1])]) : m.Case === "MapNode" ? (() => {
//     var l2 = tree_map(f, m.Fields[2]);
//     var v2 = f(m.Fields[1]);
//     var r2 = tree_map(f, m.Fields[3]);
//     return new MapTree("MapNode", [m.Fields[0], v2, l2, r2, m.Fields[4]]);
//   })() : tree_empty();
// }

function tree_mapi(f: (k: any, v: any) => any, m: MapTree): MapTree {
    return m.Case === "MapOne" ? new MapTree("MapOne", [m.Fields[0], f(m.Fields[0], m.Fields[1])]) : m.Case === "MapNode" ? new MapTree("MapNode", [m.Fields[0], f(m.Fields[0], m.Fields[1]), tree_mapi(f, m.Fields[2]), tree_mapi(f, m.Fields[3]), m.Fields[4]]) : tree_empty();
}

function tree_foldBack(f: (k: any, v: any, acc: any) => any, m: MapTree, x: any): any {
    return m.Case === "MapOne" ? f(m.Fields[0], m.Fields[1], x) : m.Case === "MapNode" ? tree_foldBack(f, m.Fields[2], f(m.Fields[0], m.Fields[1], tree_foldBack(f, m.Fields[3], x))) : x;
}

function tree_fold(f: (acc: any, k: any, v: any) => any, x: any, m: MapTree): any {
    return m.Case === "MapOne" ? f(x, m.Fields[0], m.Fields[1]) : m.Case === "MapNode" ? tree_fold(f, f(tree_fold(f, x, m.Fields[2]), m.Fields[0], m.Fields[1]), m.Fields[3]) : x;
}

// function tree_foldFromTo(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any): any {
//   if (m.Case === "MapOne") {
//     var cLoKey = comparer.Compare(lo, m.Fields[0]);
//     var cKeyHi = comparer.Compare(m.Fields[0], hi);
//     var x_1 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.Fields[0], m.Fields[1], x) : x;
//     return x_1;
//   }
//   else if (m.Case === "MapNode") {
//     var cLoKey = comparer.Compare(lo, m.Fields[0]);
//     var cKeyHi = comparer.Compare(m.Fields[0], hi);
//     var x_1 = cLoKey < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.Fields[2], x) : x;
//     var x_2 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.Fields[0], m.Fields[1], x_1) : x_1;
//     var x_3 = cKeyHi < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.Fields[3], x_2) : x_2;
//     return x_3;
//   }
//   return x;
// }

// function tree_foldSection(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any) {
//   return comparer.Compare(lo, hi) === 1 ? x : tree_foldFromTo(comparer, lo, hi, f, m, x);
// }

// function tree_loop(m: MapTree, acc: any): List<[any,any]> {
//   return m.Case === "MapOne"
//     ? new List([m.Fields[0], m.Fields[1]], acc)
//     : m.Case === "MapNode"
//       ? tree_loop(m.Fields[2], new List([m.Fields[0], m.Fields[1]], tree_loop(m.Fields[3], acc)))
//       : acc;
// }

// function tree_toList(m: MapTree) {
//   return tree_loop(m, new List());
// }

// function tree_toArray(m: MapTree) {
//   return Array.from(tree_toList(m));
// }

// function tree_ofList(comparer: IComparer<any>, l: List<[any,any]>) {
//   return Seq.fold((acc: MapTree, tupledArg: [any, any]) => {
//     return tree_add(comparer, tupledArg[0], tupledArg[1], acc);
//   }, tree_empty(), l);
// }

function tree_mkFromEnumerator(comparer: IComparer<any>, acc: MapTree, e: Iterator<any>): MapTree {
    let cur = e.next();
    while (!cur.done) {
        acc = tree_add(comparer, cur.value[0], cur.value[1], acc);
        cur = e.next();
    }
    return acc;
}

// function tree_ofArray(comparer: IComparer<any>, arr: ArrayLike<[any,any]>) {
//   var res = tree_empty();
//   for (var i = 0; i <= arr.length - 1; i++) {
//     res = tree_add(comparer, arr[i][0], arr[i][1], res);
//   }
//   return res;
// }

function tree_ofSeq(comparer: IComparer<any>, c: Iterable<any>): MapTree {
    var ie = c[Symbol.iterator]();
    return tree_mkFromEnumerator(comparer, tree_empty(), ie);
}

// function tree_copyToArray(s: MapTree, arr: ArrayLike<any>, i: number) {
//   tree_iter((x, y) => { arr[i++] = [x, y]; }, s);
// }

function tree_collapseLHS(stack: List<MapTree>): List<MapTree> {
    if (stack.tail != null) {
        if (stack.head.Case === "MapOne") {
            return stack;
        }
        else if (stack.head.Case === "MapNode") {
            return tree_collapseLHS(listOfArray([
                stack.head.Fields[2],
                new MapTree("MapOne", [stack.head.Fields[0], stack.head.Fields[1]]),
                stack.head.Fields[3]
            ], stack.tail));
        }
        else {
            return tree_collapseLHS(stack.tail);
        }
    }
    else {
        return new List<MapTree>();
    }
}

function tree_mkIterator(s: MapTree): MapIterator {
    return { stack: tree_collapseLHS(new List<MapTree>(s, new List<MapTree>())), started: false };
}

function tree_moveNext(i: MapIterator): IteratorResult<[any, any]> {
    function current(i: MapIterator): [any, any] {
        if (i.stack.tail == null) {
            return null;
        }
        else if (i.stack.head.Case === "MapOne") {
            return [i.stack.head.Fields[0], i.stack.head.Fields[1]];
        }
        throw new Error("Please report error: Map iterator, unexpected stack for current");
    }
    if (i.started) {
        if (i.stack.tail == null) {
            return { done: true, value: null };
        } else {
            if (i.stack.head.Case === "MapOne") {
                i.stack = tree_collapseLHS(i.stack.tail);
                return {
                    done: i.stack.tail == null,
                    value: current(i)
                };
            } else {
                throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
            }
        }
    }
    else {
        i.started = true;
        return {
            done: i.stack.tail == null,
            value: current(i)
        };
    };
}

export default class FMap<K,V> implements IEquatable<FMap<K,V>>, IComparable<FMap<K,V>>, Iterable<[K,V]> {
  // TODO: These should be made internal, once TypeScript accepts that modifier
  tree: MapTree;
  comparer: IComparer<K>;

  /** Do not call, use Map.create instead. */
  constructor () {}

  ToString() {
    return "map [" + Array.from(this).map(toString).join("; ") + "]";
  }

  Equals(m2: FMap<K,V>) {
    return this.CompareTo(m2) === 0;
  }

  CompareTo(m2: FMap<K,V>) {
    return this === m2 ? 0 : seqCompareWith((kvp1, kvp2) => {
      var c = this.comparer.Compare(kvp1[0], kvp2[0]);
      return c !== 0 ? c : compare(kvp1[1], kvp2[1]);
    }, this, m2);
  }

  [Symbol.iterator](): Iterator<[K,V]> {
    let i = tree_mkIterator(this.tree);
    return <Iterator<[K,V]>>{
      next: () => tree_moveNext(i)
    };
  }

  entries() {
    return this[Symbol.iterator]();
  }

  keys() {
    return seqMap(kv => kv[0], this);
  }

  values() {
    return seqMap(kv => kv[1], this);
  }

  get(k: K): V {
    return tree_find(this.comparer, k, this.tree);
  }

  has(k: K): boolean {
    return tree_mem(this.comparer, k, this.tree);
  }

  /** Not supported */
  set(k: K, v: V): FMap<K,V> {
    throw new Error("not supported");
  }

  /** Not supported */
  delete(k: K): boolean {
    throw new Error("not supported");
  }

  /** Not supported */
  clear(): void {
    throw new Error("not supported");
  }

  get size() {
    return tree_size(this.tree);
  }

  [FSymbol.reflection]() {
    return {
      type: "Microsoft.FSharp.Collections.FSharpMap",
      interfaces: ["System.IEquatable", "System.IComparable", "System.Collections.Generic.IDictionary"]
    }
  }
}

function from<K, V>(comparer: IComparer<K>, tree: MapTree) {
    let map = new FMap<K, V>();
    map.tree = tree
    map.comparer = comparer || new GenericComparer<K>();
    return map;
}

export function create<K, V>(ie?: Iterable<[K, V]>, comparer?: IComparer<K>) {
    comparer = comparer || new GenericComparer<K>();
    return from(comparer, ie ? tree_ofSeq(comparer, ie) : tree_empty()) as FMap<K, V>;
}

export function add<K, V>(k: K, v: V, map: FMap<K, V>) {
    return from(map.comparer, tree_add(map.comparer, k, v, map.tree)) as FMap<K, V>;
}

export function remove<K, V>(item: K, map: FMap<K, V>) {
    return from(map.comparer, tree_remove(map.comparer, item, map.tree)) as FMap<K, V>;
}

export function containsValue<K, V>(v: V, map: Map<K, V> | FMap<K, V>) {
    return seqFold((acc, k) => acc || equals(map.get(k), v), false, map.keys());
}

export function tryGetValue<K,V>(map: Map<K,V>, key: K, defaultValue: V): [boolean, V] {
    return map.has(key) ? [true, map.get(key)] : [false, defaultValue];
}

export function exists<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    return tree_exists(f, map.tree);
}

export function find<K, V>(k: K, map: FMap<K, V>) {
    return tree_find(map.comparer, k, map.tree) as V;
}

export function tryFind<K, V>(k: K, map: FMap<K, V>) {
    return tree_tryFind(map.comparer, k, map.tree) as V;
}

export function filter<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    return from(map.comparer, tree_filter(map.comparer, f, map.tree)) as FMap<K, V>;
}

export function fold<K, V, ST>(f: (acc: ST, k: K, v: V) => ST, seed: ST, map: FMap<K, V>) {
    return tree_fold(f, seed, map.tree) as ST;
}

export function foldBack<K, V, ST>(f: (k: K, v: V, acc: ST) => ST, map: FMap<K, V>, seed: ST) {
    return tree_foldBack(f, map.tree, seed) as ST;
}

export function forAll<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    return tree_forall(f, map.tree);
}

export function isEmpty<K, V>(map: FMap<K, V>) {
    return tree_isEmpty(map.tree);
}

export function iterate<K, V>(f: (k: K, v: V) => void, map: FMap<K, V>) {
    tree_iter(f, map.tree);
}

export function map<K, T, U>(f: (k: K, v: T) => U, map: FMap<K, T>) {
    return from(map.comparer, tree_mapi(f, map.tree)) as FMap<K, U>;
}

export function partition<K, V>(f: (k: K, v: V) => boolean, map: FMap<K, V>) {
    const rs = tree_partition(map.comparer, f, map.tree);
    return [from(map.comparer, rs[0]), from(map.comparer, rs[1])] as [FMap<K, V>, FMap<K, V>];
}

export function findKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V> | FMap<K, V>) {
    return seqPick(kv => f(kv[0], kv[1]) ? kv[0] : null, map);
}

export function tryFindKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V> | FMap<K, V>) {
    return seqTryPick(kv => f(kv[0], kv[1]) ? kv[0] : null, map);
}

export function pick<K, T, U>(f: (k: K, v: T) => U, map: FMap<K, T>) {
    const res = tryPick(f, map) as U;
    if (res != null)
        return res;
    throw new Error("key not found");
}

export function tryPick<K, T, U>(f: (k: K, v: T) => U, map: FMap<K, T>) {
    return tree_tryPick(f, map.tree) as U;
}
