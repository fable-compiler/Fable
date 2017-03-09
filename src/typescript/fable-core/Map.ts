import List from "./ListClass"
import { ofArray as listOfArray } from "./ListClass"
import { IComparer } from "./Util"
import { IEquatable } from "./Util"
import { IComparable } from "./Util"
import { toString } from "./Util"
import { equals } from "./Util"
import { compare } from "./Util"
import Comparer from "./Comparer"
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
  public size: number;
  public tag: number; //"MapEmpty" | "MapOne" | "MapNode"
  public fields: any[];

  constructor(tag: number, fields?: any[]) {
    this.tag = tag | 0;
    this.fields = fields || [];
  }
}

function tree_sizeAux(acc: number, m: MapTree): number {
  sizeAux: while (true) {
    if (m.tag === 1) {
      return acc + 1 | 0;
    } else if (m.tag === 2) {
      acc = tree_sizeAux(acc + 1, m.fields[2]);
      m = m.fields[3];
      continue sizeAux;
    } else {
      return acc | 0;
    }
  }
}

function tree_size(x: MapTree) {
    return tree_sizeAux(0, x);
}

function tree_empty() {
    return new MapTree(0);
}

function tree_height(_arg1: MapTree) {
    return _arg1.tag === 1 ? 1 : _arg1.tag === 2 ? _arg1.fields[4] : 0;
}

function tree_isEmpty(m: MapTree) {
    return m.tag === 0 ? true : false;
}

function tree_mk(l: MapTree, k: any, v: any, r: MapTree) {
  const matchValue = l.tag === 0 ? r.tag === 0 ? 0 : 1 : 1;

  switch (matchValue) {
    case 0:
      return new MapTree(1, [k, v]);

    case 1:
      const hl = tree_height(l) | 0;
      const hr = tree_height(r) | 0;
      const m = (hl < hr ? hr : hl) | 0;
      return new MapTree(2, [k, v, l, r, m + 1]);
  }
    throw new Error("internal error: Map.tree_mk");

};

function tree_rebalance(t1: MapTree, k: any, v: any, t2: MapTree) {
    var t1h = tree_height(t1);
    var t2h = tree_height(t2);
    if (t2h > t1h + 2) {
        if (t2.tag === 2) {
            if (tree_height(t2.fields[2]) > t1h + 1) {
                if (t2.fields[2].tag === 2) {
                    return tree_mk(tree_mk(t1, k, v, t2.fields[2].fields[2]), t2.fields[2].fields[0], t2.fields[2].fields[1], tree_mk(t2.fields[2].fields[3], t2.fields[0], t2.fields[1], t2.fields[3]));
                } else {
                    throw new Error("rebalance");
                }
            } else {
                return tree_mk(tree_mk(t1, k, v, t2.fields[2]), t2.fields[0], t2.fields[1], t2.fields[3]);
            }
        } else {
            throw new Error("rebalance");
        }
    } else {
        if (t1h > t2h + 2) {
            if (t1.tag === 2) {
                if (tree_height(t1.fields[3]) > t2h + 1) {
                    if (t1.fields[3].tag === 2) {
                        return tree_mk(tree_mk(t1.fields[2], t1.fields[0], t1.fields[1], t1.fields[3].fields[2]), t1.fields[3].fields[0], t1.fields[3].fields[1], tree_mk(t1.fields[3].fields[3], k, v, t2));
                    } else {
                        throw new Error("rebalance");
                    }
                } else {
                    return tree_mk(t1.fields[2], t1.fields[0], t1.fields[1], tree_mk(t1.fields[3], k, v, t2));
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
    if (m.tag === 1) {
        const c = comparer.Compare(k, m.fields[0]);
        if (c < 0) {
            return new MapTree(2, [k, v, new MapTree(0), m, 2]);
        }
        else if (c === 0) {
            return new MapTree(1, [k, v]);
        }
        return new MapTree(2, [k, v, m, new MapTree(0), 2]);
    }
    else if (m.tag === 2) {
        const c = comparer.Compare(k, m.fields[0]);
        if (c < 0) {
            return tree_rebalance(tree_add(comparer, k, v, m.fields[2]), m.fields[0], m.fields[1], m.fields[3]);
        }
        else if (c === 0) {
            return new MapTree(2, [k, v, m.fields[2], m.fields[3], m.fields[4]]);
        }
        return tree_rebalance(m.fields[2], m.fields[0], m.fields[1], tree_add(comparer, k, v, m.fields[3]));
    }
    return new MapTree(1, [k, v]);
}

function tree_find(comparer: IComparer<any>, k: any, m: MapTree): any {
    const res = tree_tryFind(comparer, k, m);
    if (res != null)
        return res;
    throw new Error("key not found");
}

function tree_tryFind(comparer: IComparer<any>, k: any, m: MapTree): any {
    tryFind: while (true) {
      if (m.tag === 1) {
        const c = comparer.Compare(k, m.fields[0]) | 0;

        if (c === 0) {
            return m.fields[1];
        } else {
            return null;
        }
      } else if (m.tag === 2) {
        const c_1 = comparer.Compare(k, m.fields[0]) | 0;

        if (c_1 < 0) {
            comparer = comparer;
            k = k;
            m = m.fields[2];
            continue tryFind;
        } else if (c_1 === 0) {
            return m.fields[1];
        } else {
            comparer = comparer;
            k = k;
            m = m.fields[3];
            continue tryFind;
        }
      } else {
        return null;
      }
    }
}

function tree_partition1(comparer: IComparer<any>, f: (k: any, v: any) => boolean, k: any, v: any, acc1: MapTree, acc2: MapTree): [MapTree, MapTree] {
    return f(k, v) ? [tree_add(comparer, k, v, acc1), acc2] : [acc1, tree_add(comparer, k, v, acc2)];
}

function tree_partitionAux(comparer: IComparer<any>, f: (k: any, v: any) => boolean, s: MapTree, acc_0: MapTree, acc_1: MapTree): [MapTree, MapTree] {
    const acc: [MapTree, MapTree] = [acc_0, acc_1];
    if (s.tag === 1) {
        return tree_partition1(comparer, f, s.fields[0], s.fields[1], acc[0], acc[1]);
    }
    else if (s.tag === 2) {
        const acc_2 = tree_partitionAux(comparer, f, s.fields[3], acc[0], acc[1]);
        const acc_3 = tree_partition1(comparer, f, s.fields[0], s.fields[1], acc_2[0], acc_2[1]);
        return tree_partitionAux(comparer, f, s.fields[2], acc_3[0], acc_3[1]);
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
    return s.tag === 1 ? tree_filter1(comparer, f, s.fields[0], s.fields[1], acc) : s.tag === 2 ? tree_filterAux(comparer, f, s.fields[3], tree_filter1(comparer, f, s.fields[0], s.fields[1], tree_filterAux(comparer, f, s.fields[2], acc))) : acc;
}

function tree_filter(comparer: IComparer<any>, f: (k: any, v: any) => boolean, s: MapTree) {
    return tree_filterAux(comparer, f, s, tree_empty());
}

function tree_spliceOutSuccessor(m: MapTree): [any, any, MapTree] {
    if (m.tag === 1) {
        return [m.fields[0], m.fields[1], new MapTree(0)];
    }
    else if (m.tag === 2) {
        if (m.fields[2].tag === 0) {
            return [m.fields[0], m.fields[1], m.fields[3]];
        }
        else {
            const kvl = tree_spliceOutSuccessor(m.fields[2]);
            return [kvl[0], kvl[1], tree_mk(kvl[2], m.fields[0], m.fields[1], m.fields[3])];
        }
    }
    throw new Error("internal error: Map.spliceOutSuccessor");
}

function tree_remove(comparer: IComparer<any>, k: any, m: MapTree): MapTree {
    if (m.tag === 1) {
        const c = comparer.Compare(k, m.fields[0]);
        if (c === 0) {
            return new MapTree(0);
        } else {
            return m;
        }
    }
    else if (m.tag === 2) {
        const c = comparer.Compare(k, m.fields[0]);
        if (c < 0) {
            return tree_rebalance(tree_remove(comparer, k, m.fields[2]), m.fields[0], m.fields[1], m.fields[3]);
        } else if (c === 0) {
            if (m.fields[2].tag === 0) {
                return m.fields[3];
            } else {
                if (m.fields[3].tag === 0) {
                    return m.fields[2];
                } else {
                    const input = tree_spliceOutSuccessor(m.fields[3]);
                    return tree_mk(m.fields[2], input[0], input[1], input[2]);
                }
            }
        } else {
            return tree_rebalance(m.fields[2], m.fields[0], m.fields[1], tree_remove(comparer, k, m.fields[3]));
        }
    }
    else {
        return tree_empty();
    }
}

function tree_mem(comparer: IComparer<any>, k: any, m: MapTree): boolean {
  mem: while (true) {
    if (m.tag === 1) {
      return comparer.Compare(k, m.fields[0]) === 0;
    } else if (m.tag === 2) {
      const c = comparer.Compare(k, m.fields[0]) | 0;

      if (c < 0) {
        comparer = comparer;
        k = k;
        m = m.fields[2];
        continue mem;
      } else if (c === 0) {
        return true;
      } else {
        comparer = comparer;
        k = k;
        m = m.fields[3];
        continue mem;
      }
    } else {
      return false;
    }
  }
}

function tree_iter(f: (k: any, v: any) => void, m: MapTree): void {
    if (m.tag === 1) {
        f(m.fields[0], m.fields[1]);
    }
    else if (m.tag === 2) {
        tree_iter(f, m.fields[2]);
        f(m.fields[0], m.fields[1]);
        tree_iter(f, m.fields[3]);
    }
}

function tree_tryPick(f: (k: any, v: any) => any, m: MapTree): any {
    if (m.tag === 1) {
        return f(m.fields[0], m.fields[1]);
    }
    else if (m.tag === 2 ) {
        var matchValue = tree_tryPick(f, m.fields[2]);
        if (matchValue == null) {
            var matchValue_1 = f(m.fields[0], m.fields[1]);
            if (matchValue_1 == null) {
                return tree_tryPick(f, m.fields[3]);
            } else {
                var res = matchValue_1;
                return res;
            }
        } else {
            return matchValue;
        }
    }
    else {
        return null;
    }
}

function tree_exists(f: (k: any, v: any) => boolean, m: MapTree): boolean {
    return m.tag === 1 ? f(m.fields[0], m.fields[1]) : m.tag === 2 ? (tree_exists(f, m.fields[2]) ? true : f(m.fields[0], m.fields[1])) ? true : tree_exists(f, m.fields[3]) : false;
}

function tree_forall(f: (k: any, v: any) => boolean, m: MapTree): boolean {
    return m.tag === 1 ? f(m.fields[0], m.fields[1]) : m.tag === 2 ? (tree_forall(f, m.fields[2]) ? f(m.fields[0], m.fields[1]) : false) ? tree_forall(f, m.fields[3]) : false : true;
}

function tree_mapi(f: (k: any, v: any) => any, m: MapTree): MapTree {
    return m.tag === 1 ? new MapTree(1, [m.fields[0], f(m.fields[0], m.fields[1])]) : m.tag === 2 ? new MapTree(2, [m.fields[0], f(m.fields[0], m.fields[1]), tree_mapi(f, m.fields[2]), tree_mapi(f, m.fields[3]), m.fields[4]]) : tree_empty();
}

function tree_foldBack(f: (k: any, v: any, acc: any) => any, m: MapTree, x: any): any {
    return m.tag === 1 ? f(m.fields[0], m.fields[1], x) : m.tag === 2 ? tree_foldBack(f, m.fields[2], f(m.fields[0], m.fields[1], tree_foldBack(f, m.fields[3], x))) : x;
}

function tree_fold(f: (acc: any, k: any, v: any) => any, x: any, m: MapTree): any {
    return m.tag === 1 ? f(x, m.fields[0], m.fields[1]) : m.tag === 2 ? tree_fold(f, f(tree_fold(f, x, m.fields[2]), m.fields[0], m.fields[1]), m.fields[3]) : x;
}

// function tree_foldFromTo(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any): any {
//   if (m.tag === 1) {
//     var cLoKey = comparer.Compare(lo, m.fields[0]);
//     var cKeyHi = comparer.Compare(m.fields[0], hi);
//     var x_1 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.fields[0], m.fields[1], x) : x;
//     return x_1;
//   }
//   else if (m.tag === 2) {
//     var cLoKey = comparer.Compare(lo, m.fields[0]);
//     var cKeyHi = comparer.Compare(m.fields[0], hi);
//     var x_1 = cLoKey < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.fields[2], x) : x;
//     var x_2 = (cLoKey <= 0 ? cKeyHi <= 0 : false) ? f(m.fields[0], m.fields[1], x_1) : x_1;
//     var x_3 = cKeyHi < 0 ? tree_foldFromTo(comparer, lo, hi, f, m.fields[3], x_2) : x_2;
//     return x_3;
//   }
//   return x;
// }

// function tree_foldSection(comparer: IComparer<any>, lo: any, hi: any, f: (k:any, v:any, acc: any) => any, m: MapTree, x: any) {
//   return comparer.Compare(lo, hi) === 1 ? x : tree_foldFromTo(comparer, lo, hi, f, m, x);
// }

// function tree_loop(m: MapTree, acc: any): List<[any,any]> {
//   return m.tag === 1
//     ? new List([m.fields[0], m.fields[1]], acc)
//     : m.tag === 2
//       ? tree_loop(m.fields[2], new List([m.fields[0], m.fields[1]], tree_loop(m.fields[3], acc)))
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
        if (stack.head.tag === 1) {
            return stack;
        }
        else if (stack.head.tag === 2) {
            return tree_collapseLHS(listOfArray([
                stack.head.fields[2],
                new MapTree(1, [stack.head.fields[0], stack.head.fields[1]]),
                stack.head.fields[3]
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
        else if (i.stack.head.tag === 1) {
            return [i.stack.head.fields[0], i.stack.head.fields[1]];
        }
        throw new Error("Please report error: Map iterator, unexpected stack for current");
    }
    if (i.started) {
        if (i.stack.tail == null) {
            return { done: true, value: null };
        } else {
            if (i.stack.head.tag === 1) {
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

export default class FableMap<K,V> implements IEquatable<FableMap<K,V>>, IComparable<FableMap<K,V>>, Iterable<[K,V]> {
  // TODO: These should be made internal, once TypeScript accepts that modifier
  tree: MapTree;
  comparer: IComparer<K>;

  /** Do not call, use Map.create instead. */
  constructor () {}

  ToString() {
    return "map [" + Array.from(this).map(toString).join("; ") + "]";
  }

  Equals(m2: FableMap<K,V>) {
    return this.CompareTo(m2) === 0;
  }

  CompareTo(m2: FableMap<K,V>) {
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

  /** Mutating method */
  set(k: K, v: V)  {
    this.tree = tree_add(this.comparer, k, v, this.tree);
  }

  /** Mutating method */
  delete(k: K): boolean {
    // TODO: Is calculating the size twice is more performant than calling tree_mem?
    const oldSize = tree_size(this.tree);
    this.tree = tree_remove(this.comparer, k, this.tree);
    return oldSize > tree_size(this.tree);
  }

  /** Mutating method */
  clear(): void {
    this.tree = tree_empty();
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
    let map = new FableMap<K, V>();
    map.tree = tree
    map.comparer = comparer || new Comparer<K>();
    return map;
}

export function create<K, V>(ie?: Iterable<[K, V]>, comparer?: IComparer<K>) {
    comparer = comparer || new Comparer<K>();
    return from(comparer, ie ? tree_ofSeq(comparer, ie) : tree_empty()) as FableMap<K, V>;
}

export function add<K, V>(k: K, v: V, map: FableMap<K, V>) {
    return from(map.comparer, tree_add(map.comparer, k, v, map.tree)) as FableMap<K, V>;
}

export function remove<K, V>(item: K, map: FableMap<K, V>) {
    return from(map.comparer, tree_remove(map.comparer, item, map.tree)) as FableMap<K, V>;
}

export function containsValue<K, V>(v: V, map: Map<K, V> | FableMap<K, V>) {
    return seqFold((acc, k) => acc || equals(map.get(k), v), false, map.keys());
}

export function tryGetValue<K,V>(map: Map<K,V>, key: K, defaultValue: V): [boolean, V] {
    return map.has(key) ? [true, map.get(key)] : [false, defaultValue];
}

export function exists<K, V>(f: (k: K, v: V) => boolean, map: FableMap<K, V>) {
    return tree_exists(f, map.tree);
}

export function find<K, V>(k: K, map: FableMap<K, V>) {
    return tree_find(map.comparer, k, map.tree) as V;
}

export function tryFind<K, V>(k: K, map: FableMap<K, V>) {
    return tree_tryFind(map.comparer, k, map.tree) as V;
}

export function filter<K, V>(f: (k: K, v: V) => boolean, map: FableMap<K, V>) {
    return from(map.comparer, tree_filter(map.comparer, f, map.tree)) as FableMap<K, V>;
}

export function fold<K, V, ST>(f: (acc: ST, k: K, v: V) => ST, seed: ST, map: FableMap<K, V>) {
    return tree_fold(f, seed, map.tree) as ST;
}

export function foldBack<K, V, ST>(f: (k: K, v: V, acc: ST) => ST, map: FableMap<K, V>, seed: ST) {
    return tree_foldBack(f, map.tree, seed) as ST;
}

export function forAll<K, V>(f: (k: K, v: V) => boolean, map: FableMap<K, V>) {
    return tree_forall(f, map.tree);
}

export function isEmpty<K, V>(map: FableMap<K, V>) {
    return tree_isEmpty(map.tree);
}

export function iterate<K, V>(f: (k: K, v: V) => void, map: FableMap<K, V>) {
    tree_iter(f, map.tree);
}

export function map<K, T, U>(f: (k: K, v: T) => U, map: FableMap<K, T>) {
    return from(map.comparer, tree_mapi(f, map.tree)) as FableMap<K, U>;
}

export function partition<K, V>(f: (k: K, v: V) => boolean, map: FableMap<K, V>) {
    const rs = tree_partition(map.comparer, f, map.tree);
    return [from(map.comparer, rs[0]), from(map.comparer, rs[1])] as [FableMap<K, V>, FableMap<K, V>];
}

export function findKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V> | FableMap<K, V>) {
    return seqPick(kv => f(kv[0], kv[1]) ? kv[0] : null, map);
}

export function tryFindKey<K, V>(f: (k: K, v: V) => boolean, map: Map<K, V> | FableMap<K, V>) {
    return seqTryPick(kv => f(kv[0], kv[1]) ? kv[0] : null, map);
}

export function pick<K, T, U>(f: (k: K, v: T) => U, map: FableMap<K, T>) {
    const res = tryPick(f, map) as U;
    if (res != null)
        return res;
    throw new Error("key not found");
}

export function tryPick<K, T, U>(f: (k: K, v: T) => U, map: FableMap<K, T>) {
    return tree_tryPick(f, map.tree) as U;
}
