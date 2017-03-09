import List from "./List"
import { ofArray as listOfArray } from "./List"
import { IComparer } from "./Util"
import { IEquatable } from "./Util"
import { IComparable } from "./Util"
import { toString } from "./Util"
import Comparer from "./Comparer"
import FSymbol from "./Symbol"
import { iterate as seqIterate } from "./Seq"
import { fold as seqFold } from "./Seq"
import { reduce as seqReduce } from "./Seq"
import { forAll as seqForAll } from "./Seq"
import { exists as seqExists } from "./Seq"
import { choose as seqChoose } from "./Seq"
import { scan as seqScan } from "./Seq"

// ----------------------------------------------
// These functions belong to Seq.ts but are
// implemented here to prevent cyclic dependencies

export function distinctBy<T, K>(f: (x: T) => K, xs: Iterable<T>) {
  return seqChoose(
    (tup: [T, FableSet<K>]) => tup[0],
    seqScan((tup: [T, FableSet<K>], x: T) => {
      const acc = tup[1];
      const k = f(x);
      return acc.has(k) ? [<T>null, acc] : [x, add(k, acc)];
    }, [<T>null, create<K>()] as [T, FableSet<K>], xs));
}

export function distinct<T>(xs: Iterable<T>) {
  return distinctBy(x => x, xs);
}
// ----------------------------------------------

interface SetIterator {
  stack: List<SetTree>;
  started: boolean;
}

export class SetTree {
  public tag: number; //"SetEmpty" | "SetOne" | "SetNode"
  public fields: any[];

  constructor(tag: number, fields?: any[]) {
    this.tag = tag | 0;
    this.fields = fields || [];
  }
}

const tree_tolerance = 2;

function tree_countAux(s: SetTree, acc: number): number {
  countAux: while (true) {
    if (s.tag === 1) {
      return acc + 1 | 0;
    } else if (s.tag === 0) {
      return acc | 0;
    } else {
      const _var5 = s.fields[1];
      acc = tree_countAux(s.fields[2], acc + 1);
      s = _var5;
      continue countAux;
    }
  }
}

function tree_count(s: SetTree) {
  return tree_countAux(s, 0);
}

function tree_SetOne(n: any) {
  return new SetTree(1, [n]);
}

function tree_SetNode(x: any, l: SetTree, r: SetTree, h: number) {
  return new SetTree(2, [x, l, r, h]);
}

function tree_height(t: SetTree): number {
  return t.tag === 1 ? 1 : t.tag === 2 ? t.fields[3] : 0;
}

function tree_mk(l: SetTree, k: any, r: SetTree) {
  const matchValue = l.tag === 0 ? r.tag === 0 ? 0 : 1 : 1;

  switch (matchValue) {
    case 0:
      return tree_SetOne(k);

    case 1:
      const hl = tree_height(l) | 0;
      const hr = tree_height(r) | 0;
      const m = (hl < hr ? hr : hl) | 0;
      return tree_SetNode(k, l, r, m + 1);
  }

  throw new Error("internal error: Set.tree_mk")
}

function tree_rebalance(t1: SetTree, k: any, t2: SetTree) {
  var t1h = tree_height(t1);
  var t2h = tree_height(t2);
  if (t2h > t1h + tree_tolerance) {
    if (t2.tag === 2) {
      if (tree_height(t2.fields[1]) > t1h + 1) {
        if (t2.fields[1].tag === 2) {
          return tree_mk(tree_mk(t1, k, t2.fields[1].fields[1]), t2.fields[1].fields[0], tree_mk(t2.fields[1].fields[2], t2.fields[0], t2.fields[2]));
        } else {
          throw new Error("rebalance");
        }
      } else {
        return tree_mk(tree_mk(t1, k, t2.fields[1]), t2.fields[0], t2.fields[2]);
      }
    } else {
      throw new Error("rebalance");
    }
  } else {
    if (t1h > t2h + tree_tolerance) {
      if (t1.tag === 2) {
        if (tree_height(t1.fields[2]) > t2h + 1) {
          if (t1.fields[2].tag === 2) {
            return tree_mk(tree_mk(t1.fields[1], t1.fields[0], t1.fields[2].fields[1]), t1.fields[2].fields[0], tree_mk(t1.fields[2].fields[2], k, t2));
          } else {
            throw new Error("rebalance");
          }
        } else {
          return tree_mk(t1.fields[1], t1.fields[0], tree_mk(t1.fields[2], k, t2));
        }
      } else {
        throw new Error("rebalance");
      }
    } else {
      return tree_mk(t1, k, t2);
    }
  }
}

function tree_add(comparer: IComparer<any>, k: any, t: SetTree): SetTree {
  if (t.tag === 1) {
    const c = comparer.Compare(k, t.fields[0]);
    if (c < 0) {
      return tree_SetNode(k, new SetTree(0), t, 2);
    } else if (c === 0) {
      return t;
    } else {
      return tree_SetNode(k, t, new SetTree(0), 2);
    }
  } else if (t.tag === 0) {
    return tree_SetOne(k);
  } else {
    const c = comparer.Compare(k, t.fields[0]);
    if (c < 0) {
      return tree_rebalance(tree_add(comparer, k, t.fields[1]), t.fields[0], t.fields[2]);
    } else if (c === 0) {
      return t;
    } else {
      return tree_rebalance(t.fields[1], t.fields[0], tree_add(comparer, k, t.fields[2]));
    }
  }
}

function tree_balance(comparer: IComparer<any>, t1: SetTree, k: any, t2: SetTree): SetTree {
  const matchValue: any[] = t1.tag === 2 ? t2.tag === 0 ? [1, t1] : t2.tag === 2 ? [2, t1.fields[0], t2] : [2, t1.fields[0], t2] : t1.tag === 1 ? t2.tag === 2 ? [3, t2.fields[0], t1] : t2.tag === 1 ? [4, t1.fields[3], t2.fields[3], t1.fields[0], t2.fields[0], t1.fields[1], t1.fields[2], t2.fields[1], t2.fields[2]] : [1, t1] : [0, t2];

  switch (matchValue[0]) {
    case 0:
      return tree_add(comparer, k, matchValue[1]);

    case 1:
      return tree_add(comparer, k, matchValue[1]);

    case 2:
      return tree_add(comparer, k, tree_add(comparer, matchValue[1], matchValue[2]));

    case 3:
      return tree_add(comparer, k, tree_add(comparer, matchValue[1], matchValue[2]));

    case 4:
      if (matchValue[1] + tree_tolerance < matchValue[2]) {
        return tree_rebalance(tree_balance(comparer, t1, k, matchValue[7]), matchValue[4], matchValue[8]);
      } else if (matchValue[2] + tree_tolerance < matchValue[1]) {
        return tree_rebalance(matchValue[5], matchValue[3], tree_balance(comparer, matchValue[6], k, t2));
      } else {
        return tree_mk(t1, k, t2);
      }
  }

  throw new Error("internal error: Set.tree_balance");
}

function tree_split(comparer: IComparer<any>, pivot: any, t: SetTree): any { // [SetTree, boolean, SetTree] {
  if (t.tag === 1) {
    const c = comparer.Compare(t.fields[0], pivot);

    if (c < 0) {
      return [t, false, new SetTree(0)];
    } else if (c === 0) {
      return [new SetTree(0), true, new SetTree(0)];
    } else {
      return [new SetTree(0), false, t];
    }
  } else if (t.tag === 0) {
    return [new SetTree(0), false, new SetTree(0)];
  } else {
    const c = comparer.Compare(pivot, t.fields[0]);

    if (c < 0) {
      const patternInput = tree_split(comparer, pivot, t.fields[1]);
      return [patternInput[0], patternInput[1], tree_balance(comparer, patternInput[2], t.fields[0], t.fields[2])];
    } else if (c === 0) {
      return [t.fields[1], true, t.fields[2]];
    } else {
      const patternInput = tree_split(comparer, pivot, t.fields[2]);
      return [tree_balance(comparer, t.fields[1], t.fields[0], patternInput[0]), patternInput[1], patternInput[2]];
    }
  }
}

function tree_spliceOutSuccessor(t: SetTree): any { // [any,SetTree] {
  if (t.tag === 1) {
    return [t.fields[0], new SetTree(0)];
  } else if (t.tag === 2) {
    if (t.fields[1].tag === 0) {
      return [t.fields[0], t.fields[2]];
    } else {
      const patternInput = tree_spliceOutSuccessor(t.fields[1]);
      return [patternInput[0], tree_mk(patternInput[1], t.fields[0], t.fields[2])];
    }
  } else {
    throw new Error("internal error: Map.spliceOutSuccessor");
  }
}

function tree_remove(comparer: IComparer<any>, k: any, t: SetTree): SetTree {
  if (t.tag === 1) {
    const c = comparer.Compare(k, t.fields[0]);

    if (c === 0) {
      return new SetTree(0);
    } else {
      return t;
    }
  } else if (t.tag === 2) {
    const c = comparer.Compare(k, t.fields[0]);

    if (c < 0) {
      return tree_rebalance(tree_remove(comparer, k, t.fields[1]), t.fields[0], t.fields[2]);
    } else if (c === 0) {
      const matchValue = [t.fields[1], t.fields[2]];

      if (matchValue[0].tag === 0) {
        return t.fields[2];
      } else if (matchValue[1].tag === 0) {
        return t.fields[1];
      } else {
        const patternInput = tree_spliceOutSuccessor(t.fields[2]);
        return tree_mk(t.fields[1], patternInput[0], patternInput[1]);
      }
    } else {
      return tree_rebalance(t.fields[1], t.fields[0], tree_remove(comparer, k, t.fields[2]));
    }
  } else {
    return t;
  }
}

function tree_mem(comparer: IComparer<any>, k: any, t: SetTree): boolean {
  mem: while (true) {
    if (t.tag === 1) {
      return comparer.Compare(k, t.fields[0]) === 0;
    } else if (t.tag === 0) {
      return false;
    } else {
      const c = comparer.Compare(k, t.fields[0]) | 0;

      if (c < 0) {
        comparer = comparer;
        k = k;
        t = t.fields[1];
        continue mem;
      } else if (c === 0) {
        return true;
      } else {
        comparer = comparer;
        k = k;
        t = t.fields[2];
        continue mem;
      }
    }
  }
}

function tree_iter(f: (x:any)=>void, t: SetTree) {
  if (t.tag === 1) {
    f(t.fields[0]);
  } else {
    if (t.tag === 0) {} else {
      tree_iter(f, t.fields[1]);
      f(t.fields[0]);
      tree_iter(f, t.fields[2]);
    }
  }
}

function tree_foldBack(f: (x:any, acc:any)=>any, m: SetTree, x: any): any {
  return m.tag === 1 ? f(m.fields[0], x) : m.tag === 0 ? x : tree_foldBack(f, m.fields[1], f(m.fields[0], tree_foldBack(f, m.fields[2], x)));
}

function tree_fold(f: (acc:any, x:any)=>any, x: any, m: SetTree): any {
  if (m.tag === 1) {
    return f(x, m.fields[0]);
  } else if (m.tag === 0) {
    return x;
  } else {
    const x_1 = tree_fold(f, x, m.fields[1]);
    const x_2 = f(x_1, m.fields[0]);
    return tree_fold(f, x_2, m.fields[2]);
  }
}

function tree_forall(f: (x:any)=>boolean, m: SetTree): boolean {
  return m.tag === 1 ? f(m.fields[0]) : m.tag === 0 ? true : (f(m.fields[0]) ? tree_forall(f, m.fields[1]) : false) ? tree_forall(f, m.fields[2]) : false;
}

function tree_exists(f: (x:any)=>boolean, m: SetTree): boolean {
  return m.tag === 1 ? f(m.fields[0]) : m.tag === 0 ? false : (f(m.fields[0]) ? true : tree_exists(f, m.fields[1])) ? true : tree_exists(f, m.fields[2]);
}

function tree_isEmpty(m: SetTree): boolean {
  return m.tag === 0 ? true : false;
}

function tree_subset(comparer: IComparer<any>, a: SetTree, b: SetTree) {
  return tree_forall(x => tree_mem(comparer, x, b), a);
}

function tree_psubset(comparer: IComparer<any>, a: SetTree, b: SetTree) {
  return tree_forall(x => tree_mem(comparer, x, b), a) ? tree_exists(x => !tree_mem(comparer, x, a), b) : false;
}

function tree_filterAux(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree, acc: SetTree): SetTree {
  if (s.tag === 1) {
    if (f(s.fields[0])) {
      return tree_add(comparer, s.fields[0], acc);
    } else {
      return acc;
    }
  } else if (s.tag === 0) {
    return acc;
  } else {
    const acc_1 = f(s.fields[0]) ? tree_add(comparer, s.fields[0], acc) : acc;
    return tree_filterAux(comparer, f, s.fields[1], tree_filterAux(comparer, f, s.fields[2], acc_1));
  }
}

function tree_filter(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree): SetTree {
  return tree_filterAux(comparer, f, s, new SetTree(0));
}

function tree_diffAux(comparer: IComparer<any>, m: SetTree, acc: SetTree): SetTree {
  diffAux: while (true) {
    if (m.tag === 1) {
      return tree_remove(comparer, m.fields[0], acc);
    } else if (m.tag === 0) {
      return acc;
    } else {
      const _var6 = comparer;
      const _var7 = m.fields[1];
      acc = tree_diffAux(comparer, m.fields[2], tree_remove(comparer, m.fields[0], acc));
      comparer = _var6;
      m = _var7;
      continue diffAux;
    }
  }
}

function tree_diff(comparer: IComparer<any>, a: SetTree, b: SetTree): SetTree {
  return tree_diffAux(comparer, b, a);
}

function tree_union(comparer: IComparer<any>, t1: SetTree, t2: SetTree): SetTree {
  const matchValue = t1.tag === 0 ? [1, t2] : t1.tag === 1 ? t2.tag === 0 ? [2, t1] : t2.tag === 1 ? [3, t1.fields[0], t2] : [3, t1.fields[0], t2] : t2.tag === 0 ? [2, t1] : t2.tag === 1 ? [4, t2.fields[0], t1] : [0, t1.fields[3], t2.fields[3], t1.fields[0], t2.fields[0], t1.fields[1], t1.fields[2], t2.fields[1], t2.fields[2]];

  switch (matchValue[0]) {
    case 0:
      if (matchValue[1] > matchValue[2]) {
        const patternInput = tree_split(comparer, matchValue[3], t2);
        return tree_balance(comparer, tree_union(comparer, matchValue[5], patternInput[0]), matchValue[3], tree_union(comparer, matchValue[6], patternInput[2]));
      } else {
        const patternInput_1 = tree_split(comparer, matchValue[4], t1);
        return tree_balance(comparer, tree_union(comparer, matchValue[7], patternInput_1[0]), matchValue[4], tree_union(comparer, matchValue[8], patternInput_1[2]));
      }

    case 1:
      return matchValue[1];

    case 2:
      return matchValue[1];

    case 3:
      return tree_add(comparer, matchValue[1], matchValue[2]);

    case 4:
      return tree_add(comparer, matchValue[1], matchValue[2]);
  }

  throw new Error("internal error: Set.tree_union")
}

function tree_intersectionAux(comparer: IComparer<any>, b: SetTree, m: SetTree, acc: SetTree): SetTree {
  intersectionAux: while (true) {
    if (m.tag === 1) {
      if (tree_mem(comparer, m.fields[0], b)) {
        return tree_add(comparer, m.fields[0], acc);
      } else {
        return acc;
      }
    } else if (m.tag === 0) {
      return acc;
    } else {
      const acc_1 = tree_intersectionAux(comparer, b, m.fields[2], acc);
      const acc_2 = tree_mem(comparer, m.fields[0], b) ? tree_add(comparer, m.fields[0], acc_1) : acc_1;
      comparer = comparer;
      b = b;
      m = m.fields[1];
      acc = acc_2;
      continue intersectionAux;
    }
  }
}

function tree_intersection(comparer: IComparer<any>, a: SetTree, b: SetTree) {
  return tree_intersectionAux(comparer, b, a, new SetTree(0));
}

function tree_partition1(comparer: IComparer<any>, f: (x:any)=>boolean, k: any, acc1: SetTree, acc2: SetTree): [SetTree, SetTree] {
  return f(k) ? [tree_add(comparer, k, acc1), acc2] : [acc1, tree_add(comparer, k, acc2)];
}

function tree_partitionAux(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree, acc_0: SetTree, acc_1: SetTree): [SetTree, SetTree] {
  var acc = <[SetTree,SetTree]>[acc_0, acc_1];
  if (s.tag === 1) {
    return tree_partition1(comparer, f, s.fields[0], acc[0], acc[1]);
  } else if (s.tag === 0) {
    return acc;
  } else {
    const acc_2 = tree_partitionAux(comparer, f, s.fields[2], acc[0], acc[1]);
    const acc_3 = tree_partition1(comparer, f, s.fields[0], acc_2[0], acc_2[1]);
    return tree_partitionAux(comparer, f, s.fields[1], acc_3[0], acc_3[1]);
  }
}

function tree_partition(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree) {
  return tree_partitionAux(comparer, f, s, new SetTree(0), new SetTree(0));
}

// function tree_$MatchSetNode$MatchSetEmpty$(s: SetTree) {
//   return s.tag === 1 ? new Choice("Choice1Of2", [[s.fields[0], new SetTree(0), new SetTree(0)]]) : s.tag === 0 ? new Choice("Choice2Of2", [null]) : new Choice("Choice1Of2", [[s.fields[0], s.fields[1], s.fields[2]]]);
// }

function tree_minimumElementAux(s: SetTree, n: any): any {
  return s.tag === 1 ? s.fields[0] : s.tag === 0 ? n : tree_minimumElementAux(s.fields[1], s.fields[0]);
}

function tree_minimumElementOpt(s: SetTree): any {
  return s.tag === 1 ? s.fields[0] : s.tag === 0 ? null : tree_minimumElementAux(s.fields[1], s.fields[0]);
}

function tree_maximumElementAux(s: SetTree, n: any): any {
  return s.tag === 1 ? s.fields[0] : s.tag === 0 ? n : tree_maximumElementAux(s.fields[2], s.fields[0]);
}

function tree_maximumElementOpt(s: SetTree): any {
  return s.tag === 1 ? s.fields[0] : s.tag === 0 ? null : tree_maximumElementAux(s.fields[2], s.fields[0]);
}

function tree_minimumElement(s: SetTree): any {
  var matchValue = tree_minimumElementOpt(s);
  if (matchValue == null) {
    throw new Error("Set contains no elements");
  } else {
    return matchValue;
  }
}

function tree_maximumElement(s: SetTree) {
  var matchValue = tree_maximumElementOpt(s);
  if (matchValue == null) {
    throw new Error("Set contains no elements");
  } else {
    return matchValue;
  }
}

function tree_collapseLHS(stack: List<SetTree>): List<SetTree> {
  collapseLHS: while (true) {
    if (stack.tail != null) {
      if (stack.head.tag === 1) {
        return stack;
      } else if (stack.head.tag === 2) {
        stack = listOfArray([stack.head.fields[1], tree_SetOne(stack.head.fields[0]), stack.head.fields[2]], stack.tail);
        continue collapseLHS;
      } else {
        stack = stack.tail;
        continue collapseLHS;
      }
    } else {
      return new List<SetTree>();
    }
  }
}

function tree_mkIterator(s: SetTree): SetIterator {
  return { stack: tree_collapseLHS(new List<SetTree>(s, new List<SetTree>())), started: false };
};

// function tree_notStarted() {
//   throw new Error("Enumeration not started");
// };

// var alreadyFinished = $exports.alreadyFinished = function () {
//   throw new Error("Enumeration already started");
// };

function tree_moveNext(i: SetIterator): IteratorResult<any> {
  function current(i: SetIterator): any {
    if (i.stack.tail == null) {
      return null;
    }
    else if (i.stack.head.tag === 1) {
      return i.stack.head.fields[0];
    }
    throw new Error("Please report error: Set iterator, unexpected stack for current");
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
        throw new Error("Please report error: Set iterator, unexpected stack for moveNext");
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

function tree_compareStacks(comparer: IComparer<any>, l1: List<SetTree>, l2: List<SetTree>): number {
  compareStacks: while (true) {
    const matchValue = l1.tail != null ? l2.tail != null ? l2.head.tag === 1 ? l1.head.tag === 1 ? [4, l1.head.fields[0], l2.head.fields[0], l1.tail, l2.tail] : l1.head.tag === 2 ? l1.head.fields[1].tag === 0 ? [6, l1.head.fields[1], l1.head.fields[0], l1.head.fields[2], l2.head.fields[0], l1.tail, l2.tail] : [9, l1.head.fields[0], l1.head.fields[1], l1.head.fields[2], l1.tail] : [10, l2.head.fields[0], l2.tail] : l2.head.tag === 2 ? l2.head.fields[1].tag === 0 ? l1.head.tag === 1 ? [5, l1.head.fields[0], l2.head.fields[0], l2.head.fields[2], l1.tail, l2.tail] : l1.head.tag === 2 ? l1.head.fields[1].tag === 0 ? [7, l1.head.fields[0], l1.head.fields[2], l2.head.fields[0], l2.head.fields[2], l1.tail, l2.tail] : [9, l1.head.fields[0], l1.head.fields[1], l1.head.fields[2], l1.tail] : [11, l2.head.fields[0], l2.head.fields[1], l2.head.fields[2], l2.tail] : l1.head.tag === 1 ? [8, l1.head.fields[0], l1.tail] : l1.head.tag === 2 ? [9, l1.head.fields[0], l1.head.fields[1], l1.head.fields[2], l1.tail] : [11, l2.head.fields[0], l2.head.fields[1], l2.head.fields[2], l2.tail] : l1.head.tag === 1 ? [8, l1.head.fields[0], l1.tail] : l1.head.tag === 2 ? [9, l1.head.fields[0], l1.head.fields[1], l1.head.fields[2], l1.tail] : [3, l1.tail, l2.tail] : [2] : l2.tail != null ? [1] : [0];

    switch (matchValue[0]) {
      case 0:
        return 0;

      case 1:
        return -1;

      case 2:
        return 1;

      case 3:
        comparer = comparer;
        l1 = matchValue[1];
        l2 = matchValue[2];
        continue compareStacks;

      case 4:
        const c = comparer.Compare(matchValue[1], matchValue[2]) | 0;

        if (c !== 0) {
          return c | 0;
        } else {
          comparer = comparer;
          l1 = matchValue[3];
          l2 = matchValue[4];
          continue compareStacks;
        }

      case 5:
        const c_1 = comparer.Compare(matchValue[1], matchValue[2]) | 0;

        if (c_1 !== 0) {
          return c_1 | 0;
        } else {
          comparer = comparer;
          l1 = new List(new SetTree(0), matchValue[4]);
          l2 = new List(matchValue[3], matchValue[5]);
          continue compareStacks;
        }

      case 6:
        const c_2 = comparer.Compare(matchValue[2], matchValue[4]) | 0;

        if (c_2 !== 0) {
          return c_2 | 0;
        } else {
          comparer = comparer;
          l1 = new List(matchValue[3], matchValue[5]);
          l2 = new List(matchValue[1], matchValue[6]);
          continue compareStacks;
        }

      case 7:
        const c_3 = comparer.Compare(matchValue[1], matchValue[3]) | 0;

        if (c_3 !== 0) {
          return c_3 | 0;
        } else {
          comparer = comparer;
          l1 = new List(matchValue[2], matchValue[5]);
          l2 = new List(matchValue[4], matchValue[6]);
          continue compareStacks;
        }

      case 8:
        comparer = comparer;
        l1 = listOfArray([new SetTree(0), tree_SetOne(matchValue[1])], matchValue[2]);
        l2 = l2;
        continue compareStacks;

      case 9:
        comparer = comparer;
        l1 = listOfArray([matchValue[2], tree_SetNode(matchValue[1], new SetTree(0), matchValue[3], 0)], matchValue[4]);
        l2 = l2;
        continue compareStacks;

      case 10:
        comparer = comparer;
        l1 = l1;
        l2 = listOfArray([new SetTree(0), tree_SetOne(matchValue[1])], matchValue[2]);
        continue compareStacks;

      case 11:
        comparer = comparer;
        l1 = l1;
        l2 = listOfArray([matchValue[2], tree_SetNode(matchValue[1], new SetTree(0), matchValue[3], 0)], matchValue[4]);
        continue compareStacks;
    }
  }
}

function tree_compare(comparer: IComparer<any>, s1: SetTree, s2: SetTree) {
  if (s1.tag === 0) {
    return s2.tag === 0 ? 0 : -1;
  } else {
    return s2.tag === 0 ? 1 : tree_compareStacks(comparer, listOfArray([s1]), listOfArray([s2]));
  }
}

function tree_mkFromEnumerator(comparer: IComparer<any>, acc: SetTree, e: Iterator<any>): SetTree {
  let cur = e.next();
  while (!cur.done) {
    acc = tree_add(comparer, cur.value, acc);
    cur = e.next();
  }
  return acc;
}

function tree_ofSeq(comparer: IComparer<any>, c: Iterable<any>) {
  var ie = c[Symbol.iterator]();
  return tree_mkFromEnumerator(comparer, new SetTree(0), ie);
}

export default class FableSet<T> implements IEquatable<FableSet<T>>, IComparable<FableSet<T>>, Iterable<T> {
  // TODO: These should be made internal, once TypeScript accepts that modifier
  public tree: SetTree;
  public comparer: IComparer<T>;

  /** Do not call, use Set.create instead. */
  constructor () {}

  ToString() {
    return "set [" + Array.from(this).map(toString).join("; ") + "]";
  }

  Equals(s2: FableSet<T>) {
    return this.CompareTo(s2) === 0;
  }

  CompareTo(s2: FableSet<T>) {
    return this === s2 ? 0 : tree_compare(this.comparer, this.tree, s2.tree);
  }

  [Symbol.iterator](): Iterator<T> {
    let i = tree_mkIterator(this.tree);
    return <Iterator<T>>{
      next: () => tree_moveNext(i)
    };
  }

  values() {
    return this[Symbol.iterator]();
  }

  has(v: T) {
    return tree_mem(this.comparer, v, this.tree);
  }

  /** Mutating method */
  add(v: T): FableSet<T> {
    this.tree = tree_add(this.comparer, v, this.tree);
    return this;
  }

  /** Mutating method */
  delete(v: T): boolean {
    // TODO: Is calculating the size twice is more performant than calling tree_mem?
    const oldSize = tree_count(this.tree);
    this.tree = tree_remove(this.comparer, v, this.tree);
    return oldSize > tree_count(this.tree);
  }

  /** Mutating method */
  clear(): void {
    this.tree = new SetTree(0);
  }

  get size() {
    return tree_count(this.tree);
  }

  [FSymbol.reflection]() {
    return {
      type: "Microsoft.FSharp.Collections.FSharpSet",
      interfaces: ["System.IEquatable", "System.IComparable"]
    }
  }
}

function from<T>(comparer: IComparer<T>, tree: SetTree) {
  let s = new FableSet<T>();
  s.tree = tree
  s.comparer = comparer || new Comparer<T>();
  return s;
}

export function create<T>(ie?: Iterable<T>, comparer?: IComparer<T>) {
  comparer = comparer || new Comparer<T>();
  return from(comparer, ie ? tree_ofSeq(comparer, ie) : new SetTree(0));
}

export function isEmpty<T>(s: FableSet<T>) {
  return tree_isEmpty(s.tree);
}

export function add<T>(item: T, s: FableSet<T>) {
  return from(s.comparer, tree_add(s.comparer, item, s.tree));
}

export function addInPlace<T>(item: T, s: Set<T>) {
  return s.has(item) ? false : (s.add(item), true);
}

export function remove<T>(item: T, s: FableSet<T>) {
  return from(s.comparer, tree_remove(s.comparer, item, s.tree));
}

export function union<T>(set1: FableSet<T>, set2: FableSet<T>) {
  return set2.tree.tag === 0
    ? set1
    : set1.tree.tag === 0
      ? set2
      : from(set1.comparer, tree_union(set1.comparer, set1.tree, set2.tree));
}

export function op_Addition<T>(set1: FableSet<T>, set2: FableSet<T>) {
  return union(set1, set2);
}

export function unionInPlace<T>(set1: Set<T>, set2: Iterable<T>) {
  seqIterate(function (x) { set1.add(x) }, set2);
}

export function unionMany<T>(sets: Iterable<FableSet<T>>) {
  // Pass args as union(s, acc) instead of union(acc, s)
  // to discard the comparer of the first empty set
  return seqFold((acc, s) => <FableSet<T>>union(s, acc), create<T>(), sets);
}

export function difference<T>(set1: FableSet<T>, set2: FableSet<T>) {
  return set1.tree.tag === 0
    ? set1
    : set2.tree.tag === 0
      ? set1
      : from(set1.comparer, tree_diff(set1.comparer, set1.tree, set2.tree));
}

export function op_Subtraction<T>(set1: FableSet<T>, set2: FableSet<T>) {
  return difference(set1, set2);
}

export function differenceInPlace<T>(set1: Set<T>, set2: Iterable<T>) {
  seqIterate(function (x) { set1.delete(x) }, set2);
}

export function intersect<T>(set1: FableSet<T>, set2: FableSet<T>) {
  return set2.tree.tag === 0
    ? set2
    : set1.tree.tag === 0
      ? set1
      : from(set1.comparer, tree_intersection(set1.comparer, set1.tree, set2.tree));
}

export function intersectInPlace<T>(set1: Set<T>, set2: Iterable<T>) {
  const set2_ = set2 instanceof Set ? set2 : new Set(set2);
  seqIterate(function (x) { if (!set2_.has(x)) { set1.delete(x); } }, set1);
}

export function intersectMany<T>(sets: Iterable<FableSet<T>>) {
  return seqReduce((s1, s2) => <FableSet<T>>intersect(s1, s2), sets);
}

export function isProperSubsetOf<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  if (set1 instanceof FableSet && set2 instanceof FableSet) {
    return tree_psubset(set1.comparer, set1.tree, (set2 as FableSet<T>).tree);
  }
  else {
    set2 = set2 instanceof Set ? set2 : new Set(set2);
    return seqForAll(x => set2.has(x), set1) && seqExists(x => !set1.has(x), set2);
  }
}

export function isProperSubset<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  return isProperSubsetOf(set1, set2);
}

export function isSubsetOf<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  if (set1 instanceof FableSet && set2 instanceof FableSet) {
    return tree_subset(set1.comparer, set1.tree, (set2 as FableSet<T>).tree);
  }
  else {
    set2 = set2 instanceof Set ? set2 : new Set(set2);
    return seqForAll(x => set2.has(x), set1);
  }
}

export function isSubset<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  return isSubsetOf(set1, set2);
}

export function isProperSupersetOf<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  if (set1 instanceof FableSet && set2 instanceof FableSet) {
    return tree_psubset(set1.comparer, (set2 as FableSet<T>).tree, set1.tree);
  }
  else {
    return isProperSubset(set2 instanceof Set ? set2 : new Set(set2), set1);
  }
}

export function isProperSuperset<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  return isProperSupersetOf(set1, set2);
}

export function isSupersetOf<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  if (set1 instanceof FableSet && set2 instanceof FableSet) {
    return tree_subset(set1.comparer, set2.tree, set1.tree);
  }
  else {
    return isSubset(set2 instanceof Set ? set2 : new Set(set2), set1);
  }
}

export function isSuperset<T>(set1: FableSet<T> | Set<T>, set2: FableSet<T> | Set<T>) {
  return isSupersetOf(set1, set2);
}

export function copyTo<T>(xs: FableSet<T> | Set<T>, arr: ArrayLike<T>, arrayIndex?: number, count?: number) {
  if (!Array.isArray(arr) && !ArrayBuffer.isView(arr))
    throw new Error("Array is invalid");

count = count || arr.length;
  let i = arrayIndex || 0;
  const iter = xs[Symbol.iterator]();
  while (count--) {
    const el = iter.next();
    if (el.done) break;
    (arr as any)[i++] = el.value;
  }
}

export function partition<T>(f: (x: T) => boolean, s: FableSet<T>): [FableSet<T>,FableSet<T>] {
  if (s.tree.tag === 0) {
    return [s,s];
  }
  else {
    const tuple = tree_partition(s.comparer, f, s.tree);
    return [from(s.comparer, tuple[0]), from(s.comparer, tuple[1])];
  }
}

export function filter<T>(f: (x: T) => boolean, s: FableSet<T>): FableSet<T> {
  if (s.tree.tag === 0) {
    return s;
  }
  else {
    return from(s.comparer, tree_filter(s.comparer, f, s.tree));
  }
}

export function map<T,U>(f: (x: T) => U, s: FableSet<T>): FableSet<U> {
  const comparer = new Comparer<U>();
  return from(comparer, tree_fold((acc, k) => tree_add(comparer, f(k), acc), new SetTree(0), s.tree));
}

export function exists<T>(f: (x: T) => boolean, s: FableSet<T>): boolean {
  return tree_exists(f, s.tree);
}

export function forAll<T>(f: (x: T) => boolean, s: FableSet<T>): boolean {
  return tree_forall(f, s.tree);
}

export function fold<T,U>(f: (acc: U, x: T) => U, seed: U, s: FableSet<T>): U {
  return tree_fold(f, seed, s.tree);
}

export function foldBack<T,U>(f: (x: T, acc: U) => U, s: FableSet<T>, seed: U): U {
  return tree_foldBack(f, s.tree, seed);
}

export function iterate<T>(f: (v: T) => void, s: FableSet<T>) {
  tree_iter(f, s.tree);
}

export function minimumElement<T>(s: FableSet<T>): T {
  return tree_minimumElement(s.tree);
}

export function minElement<T>(s: FableSet<T>): T {
  return tree_minimumElement(s.tree);
}

export function maximumElement<T>(s: FableSet<T>): T {
  return tree_maximumElement(s.tree);
}

export function maxElement<T>(s: FableSet<T>): T {
  return tree_maximumElement(s.tree);
}