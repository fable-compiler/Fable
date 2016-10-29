import List from "./List.js"
import { ofArray as listOfArray } from "./List.js"
import { IComparer } from "./Util.js"
import { IEquatable } from "./Util.js"
import { IComparable } from "./Util.js"
import { toString } from "./Util.js"
import GenericComparer from "./GenericComparer.js"
import FSymbol from "./Symbol.js"
import { fold as seqFold } from "./Seq.js"
import { reduce as seqReduce } from "./Seq.js"
import { forAll as seqForAll } from "./Seq.js"
import { exists as seqExists } from "./Seq.js"

interface SetIterator {
  stack: List<SetTree>;
  started: boolean;
}

export class SetTree {
  public Case: string;
  public Fields: any[];

  constructor(caseName: "SetEmpty" | "SetOne" | "SetNode", fields: any[]) {
    this.Case = caseName;
    this.Fields = fields;
  }
}

const tree_tolerance = 2;

function tree_countAux(s: SetTree, acc: number): number {
  return s.Case === "SetOne" ? acc + 1 : s.Case === "SetEmpty" ? acc : tree_countAux(s.Fields[1], tree_countAux(s.Fields[2], acc + 1));
}

function tree_count(s: SetTree) {
  return tree_countAux(s, 0);
}

function tree_SetOne(n: any) {
  return new SetTree("SetOne", [n]);
}

function tree_SetNode(x: any, l: SetTree, r: SetTree, h: number) {
  return new SetTree("SetNode", [x, l, r, h]);
}

function tree_height(t: SetTree): number {
  return t.Case === "SetOne" ? 1 : t.Case === "SetNode" ? t.Fields[3] : 0;
}

function tree_mk(l: SetTree, k: any, r: SetTree) {
  var matchValue = [l, r];
  var $target1 = () => {
    var hl = tree_height(l);
    var hr = tree_height(r);
    var m = hl < hr ? hr : hl;
    return tree_SetNode(k, l, r, m + 1);
  }
  if (matchValue[0].Case === "SetEmpty") {
    if (matchValue[1].Case === "SetEmpty") {
      return tree_SetOne(k);
    } else {
      return $target1();
    }
  } else {
    return $target1();
  }
}

function tree_rebalance(t1: SetTree, k: any, t2: SetTree) {
  var t1h = tree_height(t1);
  var t2h = tree_height(t2);
  if (t2h > t1h + tree_tolerance) {
    if (t2.Case === "SetNode") {
      if (tree_height(t2.Fields[1]) > t1h + 1) {
        if (t2.Fields[1].Case === "SetNode") {
          return tree_mk(tree_mk(t1, k, t2.Fields[1].Fields[1]), t2.Fields[1].Fields[0], tree_mk(t2.Fields[1].Fields[2], t2.Fields[0], t2.Fields[2]));
        } else {
          throw "rebalance";
        }
      } else {
        return tree_mk(tree_mk(t1, k, t2.Fields[1]), t2.Fields[0], t2.Fields[2]);
      }
    } else {
      throw "rebalance";
    }
  } else {
    if (t1h > t2h + tree_tolerance) {
      if (t1.Case === "SetNode") {
        if (tree_height(t1.Fields[2]) > t2h + 1) {
          if (t1.Fields[2].Case === "SetNode") {
            return tree_mk(tree_mk(t1.Fields[1], t1.Fields[0], t1.Fields[2].Fields[1]), t1.Fields[2].Fields[0], tree_mk(t1.Fields[2].Fields[2], k, t2));
          } else {
            throw "rebalance";
          }
        } else {
          return tree_mk(t1.Fields[1], t1.Fields[0], tree_mk(t1.Fields[2], k, t2));
        }
      } else {
        throw "rebalance";
      }
    } else {
      return tree_mk(t1, k, t2);
    }
  }
}

function tree_add(comparer: IComparer<any>, k: any, t: SetTree): SetTree {
  return t.Case === "SetOne" ? (() => {
    var c = comparer.Compare(k, t.Fields[0]);
    if (c < 0) {
      return tree_SetNode(k, new SetTree("SetEmpty", []), t, 2);
    } else {
      if (c === 0) {
        return t;
      } else {
        return tree_SetNode(k, t, new SetTree("SetEmpty", []), 2);
      }
    }
  })() : t.Case === "SetEmpty" ? tree_SetOne(k) : (() => {
    var c = comparer.Compare(k, t.Fields[0]);
    if (c < 0) {
      return tree_rebalance(tree_add(comparer, k, t.Fields[1]), t.Fields[0], t.Fields[2]);
    } else {
      if (c === 0) {
        return t;
      } else {
        return tree_rebalance(t.Fields[1], t.Fields[0], tree_add(comparer, k, t.Fields[2]));
      }
    }
  })();
}

function tree_balance(comparer: IComparer<any>, t1: SetTree, k: any, t2: SetTree): SetTree {
  var matchValue = [t1, t2];
  var $target1 = (t1_1: SetTree) => tree_add(comparer, k, t1_1);
  var $target2 = (k1: any, t2_1: SetTree) => tree_add(comparer, k, tree_add(comparer, k1, t2_1));
  if (matchValue[0].Case === "SetOne") {
    if (matchValue[1].Case === "SetEmpty") {
      return $target1(matchValue[0]);
    } else {
      if (matchValue[1].Case === "SetOne") {
        return $target2(matchValue[0].Fields[0], matchValue[1]);
      } else {
        return $target2(matchValue[0].Fields[0], matchValue[1]);
      }
    }
  } else {
    if (matchValue[0].Case === "SetNode") {
      if (matchValue[1].Case === "SetOne") {
        var k2 = matchValue[1].Fields[0];
        var t1_1 = matchValue[0];
        return tree_add(comparer, k, tree_add(comparer, k2, t1_1));
      } else {
        if (matchValue[1].Case === "SetNode") {
          var h1 = matchValue[0].Fields[3];
          var h2 = matchValue[1].Fields[3];
          var k1 = matchValue[0].Fields[0];
          var k2 = matchValue[1].Fields[0];
          var t11 = matchValue[0].Fields[1];
          var t12 = matchValue[0].Fields[2];
          var t21 = matchValue[1].Fields[1];
          var t22 = matchValue[1].Fields[2];
          if (h1 + tree_tolerance < h2) {
            return tree_rebalance(tree_balance(comparer, t1, k, t21), k2, t22);
          } else {
            if (h2 + tree_tolerance < h1) {
              return tree_rebalance(t11, k1, tree_balance(comparer, t12, k, t2));
            } else {
              return tree_mk(t1, k, t2);
            }
          }
        } else {
          return $target1(matchValue[0]);
        }
      }
    } else {
      var t2_1 = matchValue[1];
      return tree_add(comparer, k, t2_1);
    }
  }
}

function tree_split(comparer: IComparer<any>, pivot: any, t: SetTree): any { // [SetTree, boolean, SetTree] {
  return t.Case === "SetOne" ? (() => {
    var c = comparer.Compare(t.Fields[0], pivot);
    if (c < 0) {
      return [t, false, new SetTree("SetEmpty", [])];
    } else {
      if (c === 0) {
        return [new SetTree("SetEmpty", []), true, new SetTree("SetEmpty", [])];
      } else {
        return [new SetTree("SetEmpty", []), false, t];
      }
    }
  })() : t.Case === "SetEmpty" ? [new SetTree("SetEmpty", []), false, new SetTree("SetEmpty", [])] : (() => {
    var c = comparer.Compare(pivot, t.Fields[0]);
    if (c < 0) {
      var patternInput = tree_split(comparer, pivot, t.Fields[1]);
      var t11Lo = patternInput[0];
      var t11Hi = patternInput[2];
      var havePivot = patternInput[1];
      return [t11Lo, havePivot, tree_balance(comparer, t11Hi, t.Fields[0], t.Fields[2])];
    } else {
      if (c === 0) {
        return [t.Fields[1], true, t.Fields[2]];
      } else {
        var patternInput = tree_split(comparer, pivot, t.Fields[2]);
        var t12Lo = patternInput[0];
        var t12Hi = patternInput[2];
        var havePivot = patternInput[1];
        return [tree_balance(comparer, t.Fields[1], t.Fields[0], t12Lo), havePivot, t12Hi];
      }
    }
  })();
}

function tree_spliceOutSuccessor(t: SetTree): any { // [any,SetTree] {
  return t.Case === "SetOne" ? [t.Fields[0], new SetTree("SetEmpty", [])] : t.Case === "SetNode" ? t.Fields[1].Case === "SetEmpty" ? [t.Fields[0], t.Fields[2]] : (() => {
    var patternInput = tree_spliceOutSuccessor(t.Fields[1]);
    var l_ = patternInput[1];
    var k3 = patternInput[0];
    return [k3, tree_mk(l_, t.Fields[0], t.Fields[2])];
  })() : (() => {
    throw "internal error: Map.spliceOutSuccessor";
  })();
}

function tree_remove(comparer: IComparer<any>, k: any, t: SetTree): SetTree {
  return t.Case === "SetOne" ? (() => {
    var c = comparer.Compare(k, t.Fields[0]);
    if (c === 0) {
      return new SetTree("SetEmpty", []);
    } else {
      return t;
    }
  })() : t.Case === "SetNode" ? (() => {
    var c = comparer.Compare(k, t.Fields[0]);
    if (c < 0) {
      return tree_rebalance(tree_remove(comparer, k, t.Fields[1]), t.Fields[0], t.Fields[2]);
    } else {
      if (c === 0) {
        var matchValue = [t.Fields[1], t.Fields[2]];
        if (matchValue[0].Case === "SetEmpty") {
          return t.Fields[2];
        } else {
          if (matchValue[1].Case === "SetEmpty") {
            return t.Fields[1];
          } else {
            var patternInput = tree_spliceOutSuccessor(t.Fields[2]);
            var sk = patternInput[0];
            var r_ = patternInput[1];
            return tree_mk(t.Fields[1], sk, r_);
          }
        }
      } else {
        return tree_rebalance(t.Fields[1], t.Fields[0], tree_remove(comparer, k, t.Fields[2]));
      }
    }
  })() : t;
}

function tree_mem(comparer: IComparer<any>, k: any, t: SetTree): boolean {
  return t.Case === "SetOne" ? comparer.Compare(k, t.Fields[0]) === 0 : t.Case === "SetEmpty" ? false : (() => {
    var c = comparer.Compare(k, t.Fields[0]);
    if (c < 0) {
      return tree_mem(comparer, k, t.Fields[1]);
    } else {
      if (c === 0) {
        return true;
      } else {
        return tree_mem(comparer, k, t.Fields[2]);
      }
    }
  })();
}

function tree_iter(f: (x:any)=>void, t: SetTree) {
  if (t.Case === "SetOne") {
    f(t.Fields[0]);
  } else {
    if (t.Case === "SetEmpty") {} else {
      tree_iter(f, t.Fields[1]);
      f(t.Fields[0]);
      tree_iter(f, t.Fields[2]);
    }
  }
}

function tree_foldBack(f: (x:any, acc:any)=>any, m: SetTree, x: any): any {
  return m.Case === "SetOne" ? f(m.Fields[0], x) : m.Case === "SetEmpty" ? x : tree_foldBack(f, m.Fields[1], f(m.Fields[0], tree_foldBack(f, m.Fields[2], x)));
}

function tree_fold(f: (acc:any, x:any)=>any, x: any, m: SetTree): any {
  return m.Case === "SetOne" ? f(x, m.Fields[0]) : m.Case === "SetEmpty" ? x : (() => {
    var x_1 = tree_fold(f, x, m.Fields[1]);
    var x_2 = f(x_1, m.Fields[0]);
    return tree_fold(f, x_2, m.Fields[2]);
  })();
}

function tree_forall(f: (x:any)=>boolean, m: SetTree): boolean {
  return m.Case === "SetOne" ? f(m.Fields[0]) : m.Case === "SetEmpty" ? true : (f(m.Fields[0]) ? tree_forall(f, m.Fields[1]) : false) ? tree_forall(f, m.Fields[2]) : false;
}

function tree_exists(f: (x:any)=>boolean, m: SetTree): boolean {
  return m.Case === "SetOne" ? f(m.Fields[0]) : m.Case === "SetEmpty" ? false : (f(m.Fields[0]) ? true : tree_exists(f, m.Fields[1])) ? true : tree_exists(f, m.Fields[2]);
}

function tree_isEmpty(m: SetTree): boolean {
  return m.Case === "SetEmpty" ? true : false;
}

function tree_subset(comparer: IComparer<any>, a: SetTree, b: SetTree) {
  return tree_forall(x => tree_mem(comparer, x, b), a);
}

function tree_psubset(comparer: IComparer<any>, a: SetTree, b: SetTree) {
  return tree_forall(x => tree_mem(comparer, x, b), a) ? tree_exists(x => !tree_mem(comparer, x, a), b) : false;
}

function tree_filterAux(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree, acc: SetTree): SetTree {
  return s.Case === "SetOne" ? f(s.Fields[0]) ? tree_add(comparer, s.Fields[0], acc) : acc : s.Case === "SetEmpty" ? acc : (() => {
    var acc_1 = f(s.Fields[0]) ? tree_add(comparer, s.Fields[0], acc) : acc;
    return tree_filterAux(comparer, f, s.Fields[1], tree_filterAux(comparer, f, s.Fields[2], acc_1));
  })();
}

function tree_filter(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree): SetTree {
  return tree_filterAux(comparer, f, s, new SetTree("SetEmpty", []));
}

function tree_diffAux(comparer: IComparer<any>, m: SetTree, acc: SetTree): SetTree {
  return m.Case === "SetOne" ? tree_remove(comparer, m.Fields[0], acc) : m.Case === "SetEmpty" ? acc : tree_diffAux(comparer, m.Fields[1], tree_diffAux(comparer, m.Fields[2], tree_remove(comparer, m.Fields[0], acc)));
}

function tree_diff(comparer: IComparer<any>, a: SetTree, b: SetTree): SetTree {
  return tree_diffAux(comparer, b, a);
}

function tree_union(comparer: IComparer<any>, t1: SetTree, t2: SetTree): SetTree {
  var matchValue = [t1, t2];
  var $target2 = (t: SetTree) => t;
  var $target3 = (k1: any, t2_1: SetTree) => tree_add(comparer, k1, t2_1);
  if (matchValue[0].Case === "SetEmpty") {
    var t = matchValue[1];
    return t;
  } else {
    if (matchValue[0].Case === "SetOne") {
      if (matchValue[1].Case === "SetEmpty") {
        return $target2(matchValue[0]);
      } else {
        if (matchValue[1].Case === "SetOne") {
          return $target3(matchValue[0].Fields[0], matchValue[1]);
        } else {
          return $target3(matchValue[0].Fields[0], matchValue[1]);
        }
      }
    } else {
      if (matchValue[1].Case === "SetEmpty") {
        return $target2(matchValue[0]);
      } else {
        if (matchValue[1].Case === "SetOne") {
          var k2 = matchValue[1].Fields[0];
          var t1_1 = matchValue[0];
          return tree_add(comparer, k2, t1_1);
        } else {
          var h1 = matchValue[0].Fields[3];
          var h2 = matchValue[1].Fields[3];
          var k1 = matchValue[0].Fields[0];
          var k2 = matchValue[1].Fields[0];
          var t11 = matchValue[0].Fields[1];
          var t12 = matchValue[0].Fields[2];
          var t21 = matchValue[1].Fields[1];
          var t22 = matchValue[1].Fields[2];
          if (h1 > h2) {
            var patternInput = tree_split(comparer, k1, t2);
            var lo = patternInput[0];
            var hi = patternInput[2];
            return tree_balance(comparer, tree_union(comparer, t11, lo), k1, tree_union(comparer, t12, hi));
          } else {
            var patternInput = tree_split(comparer, k2, t1);
            var lo = patternInput[0];
            var hi = patternInput[2];
            return tree_balance(comparer, tree_union(comparer, t21, lo), k2, tree_union(comparer, t22, hi));
          }
        }
      }
    }
  }
}

function tree_intersectionAux(comparer: IComparer<any>, b: SetTree, m: SetTree, acc: SetTree): SetTree {
  return m.Case === "SetOne" ? tree_mem(comparer, m.Fields[0], b) ? tree_add(comparer, m.Fields[0], acc) : acc : m.Case === "SetEmpty" ? acc : (() => {
    var acc_1 = tree_intersectionAux(comparer, b, m.Fields[2], acc);
    var acc_2 = tree_mem(comparer, m.Fields[0], b) ? tree_add(comparer, m.Fields[0], acc_1) : acc_1;
    return tree_intersectionAux(comparer, b, m.Fields[1], acc_2);
  })();
}

function tree_intersection(comparer: IComparer<any>, a: SetTree, b: SetTree) {
  return tree_intersectionAux(comparer, b, a, new SetTree("SetEmpty", []));
}

function tree_partition1(comparer: IComparer<any>, f: (x:any)=>boolean, k: any, acc1: SetTree, acc2: SetTree): [SetTree, SetTree] {
  return f(k) ? [tree_add(comparer, k, acc1), acc2] : [acc1, tree_add(comparer, k, acc2)];
}

function tree_partitionAux(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree, acc_0: SetTree, acc_1: SetTree): [SetTree, SetTree] {
  var acc = <[SetTree,SetTree]>[acc_0, acc_1];
  if (s.Case === "SetOne") {
    var acc1 = acc[0];
    var acc2 = acc[1];
    return tree_partition1(comparer, f, s.Fields[0], acc1, acc2);
  } else {
    if (s.Case === "SetEmpty") {
      return acc;
    } else {
      var acc_2 = (() => {
        var arg30_ = acc[0];
        var arg31_ = acc[1];
        return tree_partitionAux(comparer, f, s.Fields[2], arg30_, arg31_);
      })();
      var acc_3 = (() => {
        var acc1 = acc_2[0];
        var acc2 = acc_2[1];
        return tree_partition1(comparer, f, s.Fields[0], acc1, acc2);
      })();
      var arg30_ = acc_3[0];
      var arg31_ = acc_3[1];
      return tree_partitionAux(comparer, f, s.Fields[1], arg30_, arg31_);
    }
  }
}

function tree_partition(comparer: IComparer<any>, f: (x:any)=>boolean, s: SetTree) {
  var seed = [new SetTree("SetEmpty", []), new SetTree("SetEmpty", [])];
  var arg30_ = seed[0];
  var arg31_ = seed[1];
  return tree_partitionAux(comparer, f, s, arg30_, arg31_);
}

// function tree_$MatchSetNode$MatchSetEmpty$(s: SetTree) {
//   return s.Case === "SetOne" ? new Choice("Choice1Of2", [[s.Fields[0], new SetTree("SetEmpty", []), new SetTree("SetEmpty", [])]]) : s.Case === "SetEmpty" ? new Choice("Choice2Of2", [null]) : new Choice("Choice1Of2", [[s.Fields[0], s.Fields[1], s.Fields[2]]]);
// }

function tree_minimumElementAux(s: SetTree, n: any): any {
  return s.Case === "SetOne" ? s.Fields[0] : s.Case === "SetEmpty" ? n : tree_minimumElementAux(s.Fields[1], s.Fields[0]);
}

function tree_minimumElementOpt(s: SetTree): any {
  return s.Case === "SetOne" ? s.Fields[0] : s.Case === "SetEmpty" ? null : tree_minimumElementAux(s.Fields[1], s.Fields[0]);
}

function tree_maximumElementAux(s: SetTree, n: any): any {
  return s.Case === "SetOne" ? s.Fields[0] : s.Case === "SetEmpty" ? n : tree_maximumElementAux(s.Fields[2], s.Fields[0]);
}

function tree_maximumElementOpt(s: SetTree): any {
  return s.Case === "SetOne" ? s.Fields[0] : s.Case === "SetEmpty" ? null : tree_maximumElementAux(s.Fields[2], s.Fields[0]);
}

function tree_minimumElement(s: SetTree): any {
  var matchValue = tree_minimumElementOpt(s);
  if (matchValue == null) {
    throw "Set contains no elements";
  } else {
    return matchValue;
  }
}

function tree_maximumElement(s: SetTree) {
  var matchValue = tree_maximumElementOpt(s);
  if (matchValue == null) {
    throw "Set contains no elements";
  } else {
    return matchValue;
  }
}

function tree_collapseLHS(stack: List<SetTree>): List<SetTree> {
  return stack.tail != null
    ? stack.head.Case === "SetOne"
      ? stack
      : stack.head.Case === "SetNode"
        ? tree_collapseLHS(listOfArray([
            stack.head.Fields[1],
            tree_SetOne(stack.head.Fields[0]),
            stack.head.Fields[2]
          ], stack.tail))
        : tree_collapseLHS(stack.tail)
    : new List<SetTree>();
}

function tree_mkIterator(s: SetTree): SetIterator {
  return { stack: tree_collapseLHS(new List<SetTree>(s, new List<SetTree>())), started: false };
};

// function tree_notStarted() {
//   throw "Enumeration not started";
// };

// var alreadyFinished = $exports.alreadyFinished = function () {
//   throw "Enumeration already started";
// };

function tree_moveNext(i: SetIterator): IteratorResult<any> {
  function current(i: SetIterator): any {
    if (i.stack.tail == null) {
      return null;
    }
    else if (i.stack.head.Case === "SetOne") {
      return i.stack.head.Fields[0];
    }
    throw "Please report error: Set iterator, unexpected stack for current";
  }
  if (i.started) {
    if (i.stack.tail == null) {
      return { done: true, value: null };
    } else {
      if (i.stack.head.Case === "SetOne") {
        i.stack = tree_collapseLHS(i.stack.tail);
        return {
          done: i.stack.tail == null,
          value: current(i)
        };
      } else {
        throw "Please report error: Set iterator, unexpected stack for moveNext";
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
  var $target8 = (n1k: any, t1: List<SetTree>) => tree_compareStacks(comparer, listOfArray([new SetTree("SetEmpty", []), tree_SetOne(n1k)], t1), l2);
  var $target9 = (n1k: any, n1l: any, n1r: any, t1: List<SetTree>) => tree_compareStacks(comparer, listOfArray([n1l, tree_SetNode(n1k, new SetTree("SetEmpty", []), n1r, 0)], t1), l2);
  var $target11 = (n2k: any, n2l: any, n2r: any, t2: List<SetTree>) => tree_compareStacks(comparer, l1, listOfArray([n2l, tree_SetNode(n2k, new SetTree("SetEmpty", []), n2r, 0)], t2));
  if (l1.tail != null) {
    if (l2.tail != null) {
      if (l2.head.Case === "SetOne") {
        if (l1.head.Case === "SetOne") {
          const n1k = l1.head.Fields[0], n2k = l2.head.Fields[0], t1 = l1.tail, t2 = l2.tail, c = comparer.Compare(n1k, n2k);
          if (c !== 0) {
            return c;
          } else {
            return tree_compareStacks(comparer, t1, t2);
          }
        } else {
          if (l1.head.Case === "SetNode") {
            if (l1.head.Fields[1].Case === "SetEmpty") {
              const emp = l1.head.Fields[1], n1k = l1.head.Fields[0], n1r = l1.head.Fields[2], n2k = l2.head.Fields[0], t1 = l1.tail, t2 = l2.tail, c = comparer.Compare(n1k, n2k);
              if (c !== 0) {
                return c;
              } else {
                return tree_compareStacks(comparer, listOfArray([n1r], t1), listOfArray([emp], t2));
              }
            } else {
              return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
            }
          } else {
            const n2k = l2.head.Fields[0], t2 = l2.tail;
            return tree_compareStacks(comparer, l1, listOfArray([new SetTree("SetEmpty", []), tree_SetOne(n2k)], t2));
          }
        }
      } else {
        if (l2.head.Case === "SetNode") {
          if (l2.head.Fields[1].Case === "SetEmpty") {
            if (l1.head.Case === "SetOne") {
              const n1k = l1.head.Fields[0], n2k = l2.head.Fields[0], n2r = l2.head.Fields[2], t1 = l1.tail, t2 = l2.tail, c = comparer.Compare(n1k, n2k);
              if (c !== 0) {
                return c;
              } else {
                return tree_compareStacks(comparer, listOfArray([new SetTree("SetEmpty", [])], t1), listOfArray([n2r], t2));
              }
            } else {
              if (l1.head.Case === "SetNode") {
                if (l1.head.Fields[1].Case === "SetEmpty") {
                  const n1k = l1.head.Fields[0], n1r = l1.head.Fields[2], n2k = l2.head.Fields[0], n2r = l2.head.Fields[2], t1 = l1.tail, t2 = l2.tail, c = comparer.Compare(n1k, n2k);
                  if (c !== 0) {
                    return c;
                  } else {
                    return tree_compareStacks(comparer, listOfArray([n1r], t1), listOfArray([n2r], t2));
                  }
                } else {
                  return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
                }
              } else {
                return $target11(l2.head.Fields[0], l2.head.Fields[1], l2.head.Fields[2], l2.tail);
              }
            }
          } else {
            if (l1.head.Case === "SetOne") {
              return $target8(l1.head.Fields[0], l1.tail);
            } else {
              if (l1.head.Case === "SetNode") {
                return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
              } else {
                return $target11(l2.head.Fields[0], l2.head.Fields[1], l2.head.Fields[2], l2.tail);
              }
            }
          }
        } else {
          if (l1.head.Case === "SetOne") {
            return $target8(l1.head.Fields[0], l1.tail);
          } else {
            if (l1.head.Case === "SetNode") {
              return $target9(l1.head.Fields[0], l1.head.Fields[1], l1.head.Fields[2], l1.tail);
            } else {
              return tree_compareStacks(comparer, l1.tail, l2.tail);
            }
          }
        }
      }
    } else {
      return 1;
    }
  } else {
    if (l2.tail != null) {
      return -1;
    } else {
      return 0;
    }
  }
}

function tree_compare(comparer: IComparer<any>, s1: SetTree, s2: SetTree) {
  if (s1.Case === "SetEmpty") {
    if (s2.Case === "SetEmpty") {
      return 0;
    } else {
      return -1;
    }
  } else {
    if (s2.Case === "SetEmpty") {
      return 1;
    } else {
      return tree_compareStacks(comparer, listOfArray([s1]), listOfArray([s2]));
    }
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
  return tree_mkFromEnumerator(comparer, new SetTree("SetEmpty", []), ie);
}

export default class FSet<T> implements IEquatable<FSet<T>>, IComparable<FSet<T>>, Iterable<T> {
  // TODO: These should be made internal, once TypeScript accepts that modifier
  public tree: SetTree;
  public comparer: IComparer<T>;

  /** Do not call, use Set.create instead. */
  constructor () {}

  ToString() {
    return "set [" + Array.from(this).map(toString).join("; ") + "]";
  }

  Equals(s2: FSet<T>) {
    return this.CompareTo(s2) === 0;
  }

  CompareTo(s2: FSet<T>) {
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

  /** Not supported */
  add(v: T): FSet<T> {
    throw "not supported";
  }

  /** Not supported */
  delete(v: T): boolean {
    throw "not supported";
  }

  /** Not supported */
  clear(): void {
    throw "not supported";
  }

  get size() {
    return tree_count(this.tree);
  }

  [FSymbol.interfaces]() {
    return ["System.IEquatable", "System.IComparable"];
  }

  [FSymbol.typeName]() {
    return "Microsoft.FSharp.Collections.FSharpSet";
  }
}

function from<T>(comparer: IComparer<T>, tree: SetTree) {
  let s = new FSet<T>();
  s.tree = tree
  s.comparer = comparer || new GenericComparer<T>();
  return s;
}

export function create<T>(ie?: Iterable<T>, comparer?: IComparer<T>) {
  comparer = comparer || new GenericComparer<T>();
  return from(comparer, ie ? tree_ofSeq(comparer, ie) : new SetTree("SetEmpty", []));
}

export function isEmpty<T>(s: FSet<T>) {
  return tree_isEmpty(s.tree);
}

export function add<T>(item: T, s: FSet<T>) {
  return from(s.comparer, tree_add(s.comparer, item, s.tree));
}

export function addInPlace<T>(item: T, s: Set<T>) {
  return s.has(item) ? false : (s.add(item), true);
}

export function remove<T>(item: T, s: FSet<T>) {
  return from(s.comparer, tree_remove(s.comparer, item, s.tree));
}

export function union<T>(set1: FSet<T>, set2: FSet<T>) {
  return set2.tree.Case === "SetEmpty"
    ? set1
    : set1.tree.Case === "SetEmpty"
      ? set2
      : from(set1.comparer, tree_union(set1.comparer, set1.tree, set2.tree));
}

export function op_Addition<T>(set1: FSet<T>, set2: FSet<T>) {
  return union(set1, set2);
}

export function unionInPlace<T>(set1: Set<T>, set2: Iterable<T>) {
  for (const x of set2) { set1.add(x); }
}

export function unionMany<T>(sets: Iterable<FSet<T>>) {
  // Pass args as union(s, acc) instead of union(acc, s)
  // to discard the comparer of the first empty set
  return seqFold((acc, s) => <FSet<T>>union(s, acc), create<T>(), sets);
}

export function difference<T>(set1: FSet<T>, set2: FSet<T>) {
  return set1.tree.Case === "SetEmpty"
    ? set1
    : set2.tree.Case === "SetEmpty"
      ? set1
      : from(set1.comparer, tree_diff(set1.comparer, set1.tree, set2.tree));
}

export function op_Subtraction<T>(set1: FSet<T>, set2: FSet<T>) {
  return difference(set1, set2);
}

export function differenceInPlace<T>(set1: Set<T>, set2: Iterable<T>) {
  for (const x of set2) { set1.delete(x); }
}

export function intersect<T>(set1: FSet<T>, set2: FSet<T>) {
  return set2.tree.Case === "SetEmpty"
    ? set2
    : set1.tree.Case === "SetEmpty"
      ? set1
      : from(set1.comparer, tree_intersection(set1.comparer, set1.tree, set2.tree));
}

export function intersectInPlace<T>(set1: Set<T>, set2: Iterable<T>) {
  const set2_ = set2 instanceof Set ? set2 : new Set(set2);
  for (const x of set1) { if (!set2_.has(x)) { set1.delete(x); } }
}

export function intersectMany<T>(sets: Iterable<FSet<T>>) {
  return seqReduce((s1, s2) => <FSet<T>>intersect(s1, s2), sets);
}

export function isProperSubsetOf<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  if (set1 instanceof FSet && set2 instanceof FSet) {
    return tree_psubset(set1.comparer, set1.tree, (set2 as FSet<T>).tree);
  }
  else {
    set2 = set2 instanceof Set ? set2 : new Set(set2);
    return seqForAll(x => set2.has(x), set1) && seqExists(x => !set1.has(x), set2);
  }
}

export function isProperSubset<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  return isProperSubsetOf(set1, set2);
}

export function isSubsetOf<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  if (set1 instanceof FSet && set2 instanceof FSet) {
    return tree_subset(set1.comparer, set1.tree, (set2 as FSet<T>).tree);
  }
  else {
    set2 = set2 instanceof Set ? set2 : new Set(set2);
    return seqForAll(x => set2.has(x), set1);
  }
}

export function isSubset<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  return isSubsetOf(set1, set2);
}

export function isProperSupersetOf<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  if (set1 instanceof FSet && set2 instanceof FSet) {
    return tree_psubset(set1.comparer, (set2 as FSet<T>).tree, set1.tree);
  }
  else {
    return isProperSubset(set2 instanceof Set ? set2 : new Set(set2), set1);
  }
}

export function isProperSuperset<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  return isProperSupersetOf(set1, set2);
}

export function isSupersetOf<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  if (set1 instanceof FSet && set2 instanceof FSet) {
    return tree_subset(set1.comparer, set2.tree, set1.tree);
  }
  else {
    return isSubset(set2 instanceof Set ? set2 : new Set(set2), set1);
  }
}

export function isSuperset<T>(set1: FSet<T> | Set<T>, set2: FSet<T> | Set<T>) {
  return isSupersetOf(set1, set2);
}

export function copyTo<T>(xs: FSet<T> | Set<T>, arr: ArrayLike<T>, arrayIndex?: number, count?: number) {
  if (!Array.isArray(arr) && !ArrayBuffer.isView(arr))
    throw "Array is invalid";

count = count || arr.length;
  let i = arrayIndex || 0;
  const iter = xs[Symbol.iterator]();
  while (count--) {
    const el = iter.next();
    if (el.done) break;
    (arr as any)[i++] = el.value;
  }
}

export function partition<T>(f: (x: T) => boolean, s: FSet<T>): [FSet<T>,FSet<T>] {
  if (s.tree.Case === "SetEmpty") {
    return [s,s];
  }
  else {
    const tuple = tree_partition(s.comparer, f, s.tree);
    return [from(s.comparer, tuple[0]), from(s.comparer, tuple[1])];
  }
}

export function filter<T>(f: (x: T) => boolean, s: FSet<T>): FSet<T> {
  if (s.tree.Case === "SetEmpty") {
    return s;
  }
  else {
    return from(s.comparer, tree_filter(s.comparer, f, s.tree));
  }
}

export function map<T,U>(f: (x: T) => U, s: FSet<T>): FSet<U> {
  const comparer = new GenericComparer<U>();
  return from(comparer, tree_fold((acc, k) => tree_add(comparer, f(k), acc), new SetTree("SetEmpty", []), s.tree));
}

export function exists<T>(f: (x: T) => boolean, s: FSet<T>): boolean {
  return tree_exists(f, s.tree);
}

export function forAll<T>(f: (x: T) => boolean, s: FSet<T>): boolean {
  return tree_forall(f, s.tree);
}

export function fold<T,U>(f: (acc: U, x: T) => U, seed: U, s: FSet<T>): U {
  return tree_fold(f, seed, s.tree);
}

export function foldBack<T,U>(f: (x: T, acc: U) => U, s: FSet<T>, seed: U): U {
  return tree_foldBack(f, s.tree, seed);
}

export function iterate<T>(f: (v: T) => void, s: FSet<T>) {
  tree_iter(f, s.tree);
}

export function minimumElement<T>(s: FSet<T>): T {
  return tree_minimumElement(s.tree);
}

export function minElement<T>(s: FSet<T>): T {
  return tree_minimumElement(s.tree);
}

export function maximumElement<T>(s: FSet<T>): T {
  return tree_maximumElement(s.tree);
}

export function maxElement<T>(s: FSet<T>): T {
  return tree_maximumElement(s.tree);
}