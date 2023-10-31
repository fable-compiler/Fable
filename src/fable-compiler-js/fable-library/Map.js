import { record_type, bool_type, list_type, option_type, class_type } from "./Reflection.js";
import { some, value as value_1 } from "./Option.js";
import { structuralHash, compare, toIterator, equals, disposeSafe, getEnumerator, isArrayLike } from "./Util.js";
import { singleton, ofArrayWithTail, head, tail, isEmpty as isEmpty_1, fold as fold_1, empty as empty_1, FSharpList, cons } from "./List.js";
import { map as map_2, fill } from "./Array.js";
import { Record } from "./Types.js";
import { tryPick as tryPick_1, pick as pick_1, iterate as iterate_1, compareWith, map as map_1, unfold } from "./Seq.js";
import { format, join } from "./String.js";
export class MapTreeLeaf$2 {
    constructor(k, v) {
        this.k = k;
        this.v = v;
    }
}
export function MapTreeLeaf$2_$reflection(gen0, gen1) {
    return class_type("Map.MapTreeLeaf`2", [gen0, gen1], MapTreeLeaf$2);
}
export function MapTreeLeaf$2_$ctor_5BDDA1(k, v) {
    return new MapTreeLeaf$2(k, v);
}
export function MapTreeLeaf$2__get_Key(_) {
    return _.k;
}
export function MapTreeLeaf$2__get_Value(_) {
    return _.v;
}
export class MapTreeNode$2 extends MapTreeLeaf$2 {
    constructor(k, v, left, right, h) {
        super(k, v);
        this.left = left;
        this.right = right;
        this.h = (h | 0);
    }
}
export function MapTreeNode$2_$reflection(gen0, gen1) {
    return class_type("Map.MapTreeNode`2", [gen0, gen1], MapTreeNode$2, MapTreeLeaf$2_$reflection(gen0, gen1));
}
export function MapTreeNode$2_$ctor_Z39DE9543(k, v, left, right, h) {
    return new MapTreeNode$2(k, v, left, right, h);
}
export function MapTreeNode$2__get_Left(_) {
    return _.left;
}
export function MapTreeNode$2__get_Right(_) {
    return _.right;
}
export function MapTreeNode$2__get_Height(_) {
    return _.h;
}
export function MapTreeModule_empty() {
    return void 0;
}
export function MapTreeModule_sizeAux(acc_mut, m_mut) {
    MapTreeModule_sizeAux: while (true) {
        const acc = acc_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                acc_mut = MapTreeModule_sizeAux(acc + 1, MapTreeNode$2__get_Left(mn));
                m_mut = MapTreeNode$2__get_Right(mn);
                continue MapTreeModule_sizeAux;
            }
            else {
                return (acc + 1) | 0;
            }
        }
        else {
            return acc | 0;
        }
        break;
    }
}
export function MapTreeModule_size(x) {
    return MapTreeModule_sizeAux(0, x);
}
export function MapTreeModule_mk(l, k, v, r) {
    let mn, mn_1;
    let hl;
    const m = l;
    if (m != null) {
        const m2 = value_1(m);
        hl = ((m2 instanceof MapTreeNode$2) ? ((mn = m2, MapTreeNode$2__get_Height(mn))) : 1);
    }
    else {
        hl = 0;
    }
    let hr;
    const m_1 = r;
    if (m_1 != null) {
        const m2_1 = value_1(m_1);
        hr = ((m2_1 instanceof MapTreeNode$2) ? ((mn_1 = m2_1, MapTreeNode$2__get_Height(mn_1))) : 1);
    }
    else {
        hr = 0;
    }
    const m_2 = ((hl < hr) ? hr : hl) | 0;
    if (m_2 === 0) {
        return MapTreeLeaf$2_$ctor_5BDDA1(k, v);
    }
    else {
        return MapTreeNode$2_$ctor_Z39DE9543(k, v, l, r, m_2 + 1);
    }
}
export function MapTreeModule_rebalance(t1, k, v, t2) {
    let mn, mn_1, m_2, m2_2, mn_2, m_3, m2_3, mn_3;
    let t1h;
    const m = t1;
    if (m != null) {
        const m2 = value_1(m);
        t1h = ((m2 instanceof MapTreeNode$2) ? ((mn = m2, MapTreeNode$2__get_Height(mn))) : 1);
    }
    else {
        t1h = 0;
    }
    let t2h;
    const m_1 = t2;
    if (m_1 != null) {
        const m2_1 = value_1(m_1);
        t2h = ((m2_1 instanceof MapTreeNode$2) ? ((mn_1 = m2_1, MapTreeNode$2__get_Height(mn_1))) : 1);
    }
    else {
        t2h = 0;
    }
    if (t2h > (t1h + 2)) {
        const matchValue = value_1(t2);
        if (matchValue instanceof MapTreeNode$2) {
            const t2$0027 = matchValue;
            if (((m_2 = MapTreeNode$2__get_Left(t2$0027), (m_2 != null) ? ((m2_2 = value_1(m_2), (m2_2 instanceof MapTreeNode$2) ? ((mn_2 = m2_2, MapTreeNode$2__get_Height(mn_2))) : 1)) : 0)) > (t1h + 1)) {
                const matchValue_1 = value_1(MapTreeNode$2__get_Left(t2$0027));
                if (matchValue_1 instanceof MapTreeNode$2) {
                    const t2l = matchValue_1;
                    return MapTreeModule_mk(MapTreeModule_mk(t1, k, v, MapTreeNode$2__get_Left(t2l)), MapTreeLeaf$2__get_Key(t2l), MapTreeLeaf$2__get_Value(t2l), MapTreeModule_mk(MapTreeNode$2__get_Right(t2l), MapTreeLeaf$2__get_Key(t2$0027), MapTreeLeaf$2__get_Value(t2$0027), MapTreeNode$2__get_Right(t2$0027)));
                }
                else {
                    throw new Error("internal error: Map.rebalance");
                }
            }
            else {
                return MapTreeModule_mk(MapTreeModule_mk(t1, k, v, MapTreeNode$2__get_Left(t2$0027)), MapTreeLeaf$2__get_Key(t2$0027), MapTreeLeaf$2__get_Value(t2$0027), MapTreeNode$2__get_Right(t2$0027));
            }
        }
        else {
            throw new Error("internal error: Map.rebalance");
        }
    }
    else if (t1h > (t2h + 2)) {
        const matchValue_2 = value_1(t1);
        if (matchValue_2 instanceof MapTreeNode$2) {
            const t1$0027 = matchValue_2;
            if (((m_3 = MapTreeNode$2__get_Right(t1$0027), (m_3 != null) ? ((m2_3 = value_1(m_3), (m2_3 instanceof MapTreeNode$2) ? ((mn_3 = m2_3, MapTreeNode$2__get_Height(mn_3))) : 1)) : 0)) > (t2h + 1)) {
                const matchValue_3 = value_1(MapTreeNode$2__get_Right(t1$0027));
                if (matchValue_3 instanceof MapTreeNode$2) {
                    const t1r = matchValue_3;
                    return MapTreeModule_mk(MapTreeModule_mk(MapTreeNode$2__get_Left(t1$0027), MapTreeLeaf$2__get_Key(t1$0027), MapTreeLeaf$2__get_Value(t1$0027), MapTreeNode$2__get_Left(t1r)), MapTreeLeaf$2__get_Key(t1r), MapTreeLeaf$2__get_Value(t1r), MapTreeModule_mk(MapTreeNode$2__get_Right(t1r), k, v, t2));
                }
                else {
                    throw new Error("internal error: Map.rebalance");
                }
            }
            else {
                return MapTreeModule_mk(MapTreeNode$2__get_Left(t1$0027), MapTreeLeaf$2__get_Key(t1$0027), MapTreeLeaf$2__get_Value(t1$0027), MapTreeModule_mk(MapTreeNode$2__get_Right(t1$0027), k, v, t2));
            }
        }
        else {
            throw new Error("internal error: Map.rebalance");
        }
    }
    else {
        return MapTreeModule_mk(t1, k, v, t2);
    }
}
export function MapTreeModule_add(comparer, k, v, m) {
    if (m != null) {
        const m2 = value_1(m);
        const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
        if (m2 instanceof MapTreeNode$2) {
            const mn = m2;
            if (c < 0) {
                return MapTreeModule_rebalance(MapTreeModule_add(comparer, k, v, MapTreeNode$2__get_Left(mn)), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeNode$2__get_Right(mn));
            }
            else if (c === 0) {
                return MapTreeNode$2_$ctor_Z39DE9543(k, v, MapTreeNode$2__get_Left(mn), MapTreeNode$2__get_Right(mn), MapTreeNode$2__get_Height(mn));
            }
            else {
                return MapTreeModule_rebalance(MapTreeNode$2__get_Left(mn), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeModule_add(comparer, k, v, MapTreeNode$2__get_Right(mn)));
            }
        }
        else if (c < 0) {
            return MapTreeNode$2_$ctor_Z39DE9543(k, v, MapTreeModule_empty(), m, 2);
        }
        else if (c === 0) {
            return MapTreeLeaf$2_$ctor_5BDDA1(k, v);
        }
        else {
            return MapTreeNode$2_$ctor_Z39DE9543(k, v, m, MapTreeModule_empty(), 2);
        }
    }
    else {
        return MapTreeLeaf$2_$ctor_5BDDA1(k, v);
    }
}
export function MapTreeModule_tryFind(comparer_mut, k_mut, m_mut) {
    MapTreeModule_tryFind: while (true) {
        const comparer = comparer_mut, k = k_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
            if (c === 0) {
                return some(MapTreeLeaf$2__get_Value(m2));
            }
            else if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                comparer_mut = comparer;
                k_mut = k;
                m_mut = ((c < 0) ? MapTreeNode$2__get_Left(mn) : MapTreeNode$2__get_Right(mn));
                continue MapTreeModule_tryFind;
            }
            else {
                return void 0;
            }
        }
        else {
            return void 0;
        }
        break;
    }
}
export function MapTreeModule_find(comparer, k, m) {
    const matchValue = MapTreeModule_tryFind(comparer, k, m);
    if (matchValue == null) {
        throw new Error();
    }
    else {
        return value_1(matchValue);
    }
}
export function MapTreeModule_partition1(comparer, f, k, v, acc1, acc2) {
    if (f(k, v)) {
        return [MapTreeModule_add(comparer, k, v, acc1), acc2];
    }
    else {
        return [acc1, MapTreeModule_add(comparer, k, v, acc2)];
    }
}
export function MapTreeModule_partitionAux(comparer_mut, f_mut, m_mut, acc__mut, acc__1_mut) {
    MapTreeModule_partitionAux: while (true) {
        const comparer = comparer_mut, f = f_mut, m = m_mut, acc_ = acc__mut, acc__1 = acc__1_mut;
        const acc = [acc_, acc__1];
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                const acc_1 = MapTreeModule_partitionAux(comparer, f, MapTreeNode$2__get_Right(mn), acc[0], acc[1]);
                const acc_4 = MapTreeModule_partition1(comparer, f, MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), acc_1[0], acc_1[1]);
                comparer_mut = comparer;
                f_mut = f;
                m_mut = MapTreeNode$2__get_Left(mn);
                acc__mut = acc_4[0];
                acc__1_mut = acc_4[1];
                continue MapTreeModule_partitionAux;
            }
            else {
                return MapTreeModule_partition1(comparer, f, MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2), acc[0], acc[1]);
            }
        }
        else {
            return acc;
        }
        break;
    }
}
export function MapTreeModule_partition(comparer, f, m) {
    return MapTreeModule_partitionAux(comparer, f, m, MapTreeModule_empty(), MapTreeModule_empty());
}
export function MapTreeModule_filter1(comparer, f, k, v, acc) {
    if (f(k, v)) {
        return MapTreeModule_add(comparer, k, v, acc);
    }
    else {
        return acc;
    }
}
export function MapTreeModule_filterAux(comparer_mut, f_mut, m_mut, acc_mut) {
    MapTreeModule_filterAux: while (true) {
        const comparer = comparer_mut, f = f_mut, m = m_mut, acc = acc_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                const acc_1 = MapTreeModule_filterAux(comparer, f, MapTreeNode$2__get_Left(mn), acc);
                const acc_2 = MapTreeModule_filter1(comparer, f, MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), acc_1);
                comparer_mut = comparer;
                f_mut = f;
                m_mut = MapTreeNode$2__get_Right(mn);
                acc_mut = acc_2;
                continue MapTreeModule_filterAux;
            }
            else {
                return MapTreeModule_filter1(comparer, f, MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2), acc);
            }
        }
        else {
            return acc;
        }
        break;
    }
}
export function MapTreeModule_filter(comparer, f, m) {
    return MapTreeModule_filterAux(comparer, f, m, MapTreeModule_empty());
}
export function MapTreeModule_spliceOutSuccessor(m) {
    if (m != null) {
        const m2 = value_1(m);
        if (m2 instanceof MapTreeNode$2) {
            const mn = m2;
            if (MapTreeNode$2__get_Left(mn) == null) {
                return [MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeNode$2__get_Right(mn)];
            }
            else {
                const patternInput = MapTreeModule_spliceOutSuccessor(MapTreeNode$2__get_Left(mn));
                return [patternInput[0], patternInput[1], MapTreeModule_mk(patternInput[2], MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeNode$2__get_Right(mn))];
            }
        }
        else {
            return [MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2), MapTreeModule_empty()];
        }
    }
    else {
        throw new Error("internal error: Map.spliceOutSuccessor");
    }
}
export function MapTreeModule_remove(comparer, k, m) {
    if (m != null) {
        const m2 = value_1(m);
        const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
        if (m2 instanceof MapTreeNode$2) {
            const mn = m2;
            if (c < 0) {
                return MapTreeModule_rebalance(MapTreeModule_remove(comparer, k, MapTreeNode$2__get_Left(mn)), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeNode$2__get_Right(mn));
            }
            else if (c === 0) {
                if (MapTreeNode$2__get_Left(mn) == null) {
                    return MapTreeNode$2__get_Right(mn);
                }
                else if (MapTreeNode$2__get_Right(mn) == null) {
                    return MapTreeNode$2__get_Left(mn);
                }
                else {
                    const patternInput = MapTreeModule_spliceOutSuccessor(MapTreeNode$2__get_Right(mn));
                    return MapTreeModule_mk(MapTreeNode$2__get_Left(mn), patternInput[0], patternInput[1], patternInput[2]);
                }
            }
            else {
                return MapTreeModule_rebalance(MapTreeNode$2__get_Left(mn), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeModule_remove(comparer, k, MapTreeNode$2__get_Right(mn)));
            }
        }
        else if (c === 0) {
            return MapTreeModule_empty();
        }
        else {
            return m;
        }
    }
    else {
        return MapTreeModule_empty();
    }
}
export function MapTreeModule_change(comparer, k, u, m) {
    if (m != null) {
        const m2 = value_1(m);
        if (m2 instanceof MapTreeNode$2) {
            const mn = m2;
            const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(mn)) | 0;
            if (c < 0) {
                return MapTreeModule_rebalance(MapTreeModule_change(comparer, k, u, MapTreeNode$2__get_Left(mn)), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeNode$2__get_Right(mn));
            }
            else if (c === 0) {
                const matchValue_1 = u(some(MapTreeLeaf$2__get_Value(mn)));
                if (matchValue_1 != null) {
                    return MapTreeNode$2_$ctor_Z39DE9543(k, value_1(matchValue_1), MapTreeNode$2__get_Left(mn), MapTreeNode$2__get_Right(mn), MapTreeNode$2__get_Height(mn));
                }
                else if (MapTreeNode$2__get_Left(mn) == null) {
                    return MapTreeNode$2__get_Right(mn);
                }
                else if (MapTreeNode$2__get_Right(mn) == null) {
                    return MapTreeNode$2__get_Left(mn);
                }
                else {
                    const patternInput = MapTreeModule_spliceOutSuccessor(MapTreeNode$2__get_Right(mn));
                    return MapTreeModule_mk(MapTreeNode$2__get_Left(mn), patternInput[0], patternInput[1], patternInput[2]);
                }
            }
            else {
                return MapTreeModule_rebalance(MapTreeNode$2__get_Left(mn), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), MapTreeModule_change(comparer, k, u, MapTreeNode$2__get_Right(mn)));
            }
        }
        else {
            const c_1 = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
            if (c_1 < 0) {
                const matchValue_2 = u(void 0);
                if (matchValue_2 != null) {
                    return MapTreeNode$2_$ctor_Z39DE9543(k, value_1(matchValue_2), MapTreeModule_empty(), m, 2);
                }
                else {
                    return m;
                }
            }
            else if (c_1 === 0) {
                const matchValue_3 = u(some(MapTreeLeaf$2__get_Value(m2)));
                if (matchValue_3 != null) {
                    return MapTreeLeaf$2_$ctor_5BDDA1(k, value_1(matchValue_3));
                }
                else {
                    return MapTreeModule_empty();
                }
            }
            else {
                const matchValue_4 = u(void 0);
                if (matchValue_4 != null) {
                    return MapTreeNode$2_$ctor_Z39DE9543(k, value_1(matchValue_4), m, MapTreeModule_empty(), 2);
                }
                else {
                    return m;
                }
            }
        }
    }
    else {
        const matchValue = u(void 0);
        if (matchValue != null) {
            return MapTreeLeaf$2_$ctor_5BDDA1(k, value_1(matchValue));
        }
        else {
            return m;
        }
    }
}
export function MapTreeModule_mem(comparer_mut, k_mut, m_mut) {
    MapTreeModule_mem: while (true) {
        const comparer = comparer_mut, k = k_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            const c = comparer.Compare(k, MapTreeLeaf$2__get_Key(m2)) | 0;
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                if (c < 0) {
                    comparer_mut = comparer;
                    k_mut = k;
                    m_mut = MapTreeNode$2__get_Left(mn);
                    continue MapTreeModule_mem;
                }
                else if (c === 0) {
                    return true;
                }
                else {
                    comparer_mut = comparer;
                    k_mut = k;
                    m_mut = MapTreeNode$2__get_Right(mn);
                    continue MapTreeModule_mem;
                }
            }
            else {
                return c === 0;
            }
        }
        else {
            return false;
        }
        break;
    }
}
export function MapTreeModule_iterOpt(f_mut, m_mut) {
    MapTreeModule_iterOpt: while (true) {
        const f = f_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                MapTreeModule_iterOpt(f, MapTreeNode$2__get_Left(mn));
                f(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn));
                f_mut = f;
                m_mut = MapTreeNode$2__get_Right(mn);
                continue MapTreeModule_iterOpt;
            }
            else {
                f(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2));
            }
        }
        break;
    }
}
export function MapTreeModule_iter(f, m) {
    MapTreeModule_iterOpt(f, m);
}
export function MapTreeModule_tryPickOpt(f_mut, m_mut) {
    MapTreeModule_tryPickOpt: while (true) {
        const f = f_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                const matchValue = MapTreeModule_tryPickOpt(f, MapTreeNode$2__get_Left(mn));
                if (matchValue == null) {
                    const matchValue_1 = f(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn));
                    if (matchValue_1 == null) {
                        f_mut = f;
                        m_mut = MapTreeNode$2__get_Right(mn);
                        continue MapTreeModule_tryPickOpt;
                    }
                    else {
                        return matchValue_1;
                    }
                }
                else {
                    return matchValue;
                }
            }
            else {
                return f(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2));
            }
        }
        else {
            return void 0;
        }
        break;
    }
}
export function MapTreeModule_tryPick(f, m) {
    return MapTreeModule_tryPickOpt(f, m);
}
export function MapTreeModule_existsOpt(f_mut, m_mut) {
    MapTreeModule_existsOpt: while (true) {
        const f = f_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                if (MapTreeModule_existsOpt(f, MapTreeNode$2__get_Left(mn)) ? true : f(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn))) {
                    return true;
                }
                else {
                    f_mut = f;
                    m_mut = MapTreeNode$2__get_Right(mn);
                    continue MapTreeModule_existsOpt;
                }
            }
            else {
                return f(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2));
            }
        }
        else {
            return false;
        }
        break;
    }
}
export function MapTreeModule_exists(f, m) {
    return MapTreeModule_existsOpt(f, m);
}
export function MapTreeModule_forallOpt(f_mut, m_mut) {
    MapTreeModule_forallOpt: while (true) {
        const f = f_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                if (MapTreeModule_forallOpt(f, MapTreeNode$2__get_Left(mn)) && f(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn))) {
                    f_mut = f;
                    m_mut = MapTreeNode$2__get_Right(mn);
                    continue MapTreeModule_forallOpt;
                }
                else {
                    return false;
                }
            }
            else {
                return f(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2));
            }
        }
        else {
            return true;
        }
        break;
    }
}
export function MapTreeModule_forall(f, m) {
    return MapTreeModule_forallOpt(f, m);
}
export function MapTreeModule_map(f, m) {
    if (m != null) {
        const m2 = value_1(m);
        if (m2 instanceof MapTreeNode$2) {
            const mn = m2;
            const l2 = MapTreeModule_map(f, MapTreeNode$2__get_Left(mn));
            const v2 = f(MapTreeLeaf$2__get_Value(mn));
            const r2 = MapTreeModule_map(f, MapTreeNode$2__get_Right(mn));
            return MapTreeNode$2_$ctor_Z39DE9543(MapTreeLeaf$2__get_Key(mn), v2, l2, r2, MapTreeNode$2__get_Height(mn));
        }
        else {
            return MapTreeLeaf$2_$ctor_5BDDA1(MapTreeLeaf$2__get_Key(m2), f(MapTreeLeaf$2__get_Value(m2)));
        }
    }
    else {
        return MapTreeModule_empty();
    }
}
export function MapTreeModule_mapiOpt(f, m) {
    if (m != null) {
        const m2 = value_1(m);
        if (m2 instanceof MapTreeNode$2) {
            const mn = m2;
            const l2 = MapTreeModule_mapiOpt(f, MapTreeNode$2__get_Left(mn));
            const v2 = f(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn));
            const r2 = MapTreeModule_mapiOpt(f, MapTreeNode$2__get_Right(mn));
            return MapTreeNode$2_$ctor_Z39DE9543(MapTreeLeaf$2__get_Key(mn), v2, l2, r2, MapTreeNode$2__get_Height(mn));
        }
        else {
            return MapTreeLeaf$2_$ctor_5BDDA1(MapTreeLeaf$2__get_Key(m2), f(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2)));
        }
    }
    else {
        return MapTreeModule_empty();
    }
}
export function MapTreeModule_mapi(f, m) {
    return MapTreeModule_mapiOpt(f, m);
}
export function MapTreeModule_foldBackOpt(f_mut, m_mut, x_mut) {
    MapTreeModule_foldBackOpt: while (true) {
        const f = f_mut, m = m_mut, x = x_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                const x_1 = MapTreeModule_foldBackOpt(f, MapTreeNode$2__get_Right(mn), x);
                const x_2 = f(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), x_1);
                f_mut = f;
                m_mut = MapTreeNode$2__get_Left(mn);
                x_mut = x_2;
                continue MapTreeModule_foldBackOpt;
            }
            else {
                return f(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2), x);
            }
        }
        else {
            return x;
        }
        break;
    }
}
export function MapTreeModule_foldBack(f, m, x) {
    return MapTreeModule_foldBackOpt(f, m, x);
}
export function MapTreeModule_foldOpt(f_mut, x_mut, m_mut) {
    MapTreeModule_foldOpt: while (true) {
        const f = f_mut, x = x_mut, m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            if (m2 instanceof MapTreeNode$2) {
                const mn = m2;
                f_mut = f;
                x_mut = f(MapTreeModule_foldOpt(f, x, MapTreeNode$2__get_Left(mn)), MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn));
                m_mut = MapTreeNode$2__get_Right(mn);
                continue MapTreeModule_foldOpt;
            }
            else {
                return f(x, MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2));
            }
        }
        else {
            return x;
        }
        break;
    }
}
export function MapTreeModule_fold(f, x, m) {
    return MapTreeModule_foldOpt(f, x, m);
}
export function MapTreeModule_foldSectionOpt(comparer, lo, hi, f, m, x) {
    const foldFromTo = (f_1_mut, m_1_mut, x_1_mut) => {
        foldFromTo: while (true) {
            const f_1 = f_1_mut, m_1 = m_1_mut, x_1 = x_1_mut;
            if (m_1 != null) {
                const m2 = value_1(m_1);
                if (m2 instanceof MapTreeNode$2) {
                    const mn = m2;
                    const cLoKey = comparer.Compare(lo, MapTreeLeaf$2__get_Key(mn)) | 0;
                    const cKeyHi = comparer.Compare(MapTreeLeaf$2__get_Key(mn), hi) | 0;
                    const x_2 = (cLoKey < 0) ? foldFromTo(f_1, MapTreeNode$2__get_Left(mn), x_1) : x_1;
                    const x_3 = ((cLoKey <= 0) && (cKeyHi <= 0)) ? f_1(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn), x_2) : x_2;
                    if (cKeyHi < 0) {
                        f_1_mut = f_1;
                        m_1_mut = MapTreeNode$2__get_Right(mn);
                        x_1_mut = x_3;
                        continue foldFromTo;
                    }
                    else {
                        return x_3;
                    }
                }
                else if ((comparer.Compare(lo, MapTreeLeaf$2__get_Key(m2)) <= 0) && (comparer.Compare(MapTreeLeaf$2__get_Key(m2), hi) <= 0)) {
                    return f_1(MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2), x_1);
                }
                else {
                    return x_1;
                }
            }
            else {
                return x_1;
            }
            break;
        }
    };
    if (comparer.Compare(lo, hi) === 1) {
        return x;
    }
    else {
        return foldFromTo(f, m, x);
    }
}
export function MapTreeModule_foldSection(comparer, lo, hi, f, m, x) {
    return MapTreeModule_foldSectionOpt(comparer, lo, hi, f, m, x);
}
export function MapTreeModule_toList(m) {
    const loop = (m_1_mut, acc_mut) => {
        loop: while (true) {
            const m_1 = m_1_mut, acc = acc_mut;
            if (m_1 != null) {
                const m2 = value_1(m_1);
                if (m2 instanceof MapTreeNode$2) {
                    const mn = m2;
                    m_1_mut = MapTreeNode$2__get_Left(mn);
                    acc_mut = cons([MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn)], loop(MapTreeNode$2__get_Right(mn), acc));
                    continue loop;
                }
                else {
                    return cons([MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2)], acc);
                }
            }
            else {
                return acc;
            }
            break;
        }
    };
    return loop(m, empty_1());
}
export function MapTreeModule_copyToArray(m, arr, i) {
    let j = i;
    MapTreeModule_iter((x, y) => {
        arr[j] = [x, y];
        j = ((j + 1) | 0);
    }, m);
}
export function MapTreeModule_toArray(m) {
    const n = MapTreeModule_size(m) | 0;
    const res = fill(new Array(n), 0, n, [null, null]);
    MapTreeModule_copyToArray(m, res, 0);
    return res;
}
export function MapTreeModule_ofList(comparer, l) {
    return fold_1((acc, tupledArg) => MapTreeModule_add(comparer, tupledArg[0], tupledArg[1], acc), MapTreeModule_empty(), l);
}
export function MapTreeModule_mkFromEnumerator(comparer_mut, acc_mut, e_mut) {
    MapTreeModule_mkFromEnumerator: while (true) {
        const comparer = comparer_mut, acc = acc_mut, e = e_mut;
        if (e["System.Collections.IEnumerator.MoveNext"]()) {
            const patternInput = e["System.Collections.Generic.IEnumerator`1.get_Current"]();
            comparer_mut = comparer;
            acc_mut = MapTreeModule_add(comparer, patternInput[0], patternInput[1], acc);
            e_mut = e;
            continue MapTreeModule_mkFromEnumerator;
        }
        else {
            return acc;
        }
        break;
    }
}
export function MapTreeModule_ofArray(comparer, arr) {
    let res = MapTreeModule_empty();
    for (let idx = 0; idx <= (arr.length - 1); idx++) {
        const forLoopVar = arr[idx];
        res = MapTreeModule_add(comparer, forLoopVar[0], forLoopVar[1], res);
    }
    return res;
}
export function MapTreeModule_ofSeq(comparer, c) {
    if (isArrayLike(c)) {
        return MapTreeModule_ofArray(comparer, c);
    }
    else if (c instanceof FSharpList) {
        return MapTreeModule_ofList(comparer, c);
    }
    else {
        const ie = getEnumerator(c);
        try {
            return MapTreeModule_mkFromEnumerator(comparer, MapTreeModule_empty(), ie);
        }
        finally {
            disposeSafe(ie);
        }
    }
}
export class MapTreeModule_MapIterator$2 extends Record {
    constructor(stack, started) {
        super();
        this.stack = stack;
        this.started = started;
    }
}
export function MapTreeModule_MapIterator$2_$reflection(gen0, gen1) {
    return record_type("Map.MapTreeModule.MapIterator`2", [gen0, gen1], MapTreeModule_MapIterator$2, () => [["stack", list_type(option_type(MapTreeLeaf$2_$reflection(gen0, gen1)))], ["started", bool_type]]);
}
export function MapTreeModule_collapseLHS(stack_mut) {
    MapTreeModule_collapseLHS: while (true) {
        const stack = stack_mut;
        if (!isEmpty_1(stack)) {
            const rest = tail(stack);
            const m = head(stack);
            if (m != null) {
                const m2 = value_1(m);
                if (m2 instanceof MapTreeNode$2) {
                    const mn = m2;
                    stack_mut = ofArrayWithTail([MapTreeNode$2__get_Left(mn), MapTreeLeaf$2_$ctor_5BDDA1(MapTreeLeaf$2__get_Key(mn), MapTreeLeaf$2__get_Value(mn)), MapTreeNode$2__get_Right(mn)], rest);
                    continue MapTreeModule_collapseLHS;
                }
                else {
                    return stack;
                }
            }
            else {
                stack_mut = rest;
                continue MapTreeModule_collapseLHS;
            }
        }
        else {
            return empty_1();
        }
        break;
    }
}
export function MapTreeModule_mkIterator(m) {
    return new MapTreeModule_MapIterator$2(MapTreeModule_collapseLHS(singleton(m)), false);
}
export function MapTreeModule_notStarted() {
    throw new Error("enumeration not started");
}
export function MapTreeModule_alreadyFinished() {
    throw new Error("enumeration already finished");
}
export function MapTreeModule_current(i) {
    if (i.started) {
        const matchValue = i.stack;
        if (!isEmpty_1(matchValue)) {
            if (head(matchValue) != null) {
                const m = value_1(head(matchValue));
                if (m instanceof MapTreeNode$2) {
                    throw new Error("Please report error: Map iterator, unexpected stack for current");
                }
                else {
                    return [MapTreeLeaf$2__get_Key(m), MapTreeLeaf$2__get_Value(m)];
                }
            }
            else {
                throw new Error("Please report error: Map iterator, unexpected stack for current");
            }
        }
        else {
            return MapTreeModule_alreadyFinished();
        }
    }
    else {
        return MapTreeModule_notStarted();
    }
}
export function MapTreeModule_moveNext(i) {
    if (i.started) {
        const matchValue = i.stack;
        if (!isEmpty_1(matchValue)) {
            if (head(matchValue) != null) {
                const m = value_1(head(matchValue));
                if (m instanceof MapTreeNode$2) {
                    throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
                }
                else {
                    i.stack = MapTreeModule_collapseLHS(tail(matchValue));
                    return !isEmpty_1(i.stack);
                }
            }
            else {
                throw new Error("Please report error: Map iterator, unexpected stack for moveNext");
            }
        }
        else {
            return false;
        }
    }
    else {
        i.started = true;
        return !isEmpty_1(i.stack);
    }
}
export function MapTreeModule_mkIEnumerator(m) {
    let i = MapTreeModule_mkIterator(m);
    return {
        "System.Collections.Generic.IEnumerator`1.get_Current"() {
            return MapTreeModule_current(i);
        },
        "System.Collections.IEnumerator.get_Current"() {
            return MapTreeModule_current(i);
        },
        "System.Collections.IEnumerator.MoveNext"() {
            return MapTreeModule_moveNext(i);
        },
        "System.Collections.IEnumerator.Reset"() {
            i = MapTreeModule_mkIterator(m);
        },
        Dispose() {
        },
    };
}
export function MapTreeModule_toSeq(s) {
    return unfold((en_1) => {
        if (en_1["System.Collections.IEnumerator.MoveNext"]()) {
            return [en_1["System.Collections.Generic.IEnumerator`1.get_Current"](), en_1];
        }
        else {
            return void 0;
        }
    }, MapTreeModule_mkIEnumerator(s));
}
export function MapTreeModule_leftmost(m_mut) {
    MapTreeModule_leftmost: while (true) {
        const m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            let matchResult, nd_1;
            if (m2 instanceof MapTreeNode$2) {
                if (MapTreeNode$2__get_Height(m2) > 1) {
                    matchResult = 0;
                    nd_1 = m2;
                }
                else {
                    matchResult = 1;
                }
            }
            else {
                matchResult = 1;
            }
            switch (matchResult) {
                case 0:
                    if (MapTreeNode$2__get_Left(nd_1) == null) {
                        return [MapTreeLeaf$2__get_Key(nd_1), MapTreeLeaf$2__get_Value(nd_1)];
                    }
                    else {
                        m_mut = MapTreeNode$2__get_Left(nd_1);
                        continue MapTreeModule_leftmost;
                    }
                default:
                    return [MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2)];
            }
        }
        else {
            throw new Error();
        }
        break;
    }
}
export function MapTreeModule_rightmost(m_mut) {
    MapTreeModule_rightmost: while (true) {
        const m = m_mut;
        if (m != null) {
            const m2 = value_1(m);
            let matchResult, nd_1;
            if (m2 instanceof MapTreeNode$2) {
                if (MapTreeNode$2__get_Height(m2) > 1) {
                    matchResult = 0;
                    nd_1 = m2;
                }
                else {
                    matchResult = 1;
                }
            }
            else {
                matchResult = 1;
            }
            switch (matchResult) {
                case 0:
                    if (MapTreeNode$2__get_Right(nd_1) == null) {
                        return [MapTreeLeaf$2__get_Key(nd_1), MapTreeLeaf$2__get_Value(nd_1)];
                    }
                    else {
                        m_mut = MapTreeNode$2__get_Right(nd_1);
                        continue MapTreeModule_rightmost;
                    }
                default:
                    return [MapTreeLeaf$2__get_Key(m2), MapTreeLeaf$2__get_Value(m2)];
            }
        }
        else {
            throw new Error();
        }
        break;
    }
}
export class FSharpMap {
    constructor(comparer, tree) {
        this.comparer = comparer;
        this.tree = tree;
    }
    GetHashCode() {
        const this$ = this;
        return FSharpMap__ComputeHashCode(this$) | 0;
    }
    Equals(that) {
        const this$ = this;
        if (that instanceof FSharpMap) {
            const that_1 = that;
            const e1 = getEnumerator(this$);
            try {
                const e2 = getEnumerator(that_1);
                try {
                    const loop = () => {
                        const m1 = e1["System.Collections.IEnumerator.MoveNext"]();
                        if (m1 === e2["System.Collections.IEnumerator.MoveNext"]()) {
                            if (!m1) {
                                return true;
                            }
                            else {
                                const e1c = e1["System.Collections.Generic.IEnumerator`1.get_Current"]();
                                const e2c = e2["System.Collections.Generic.IEnumerator`1.get_Current"]();
                                if (equals(e1c[0], e2c[0]) && equals(e1c[1], e2c[1])) {
                                    return loop();
                                }
                                else {
                                    return false;
                                }
                            }
                        }
                        else {
                            return false;
                        }
                    };
                    return loop();
                }
                finally {
                    disposeSafe(e2);
                }
            }
            finally {
                disposeSafe(e1);
            }
        }
        else {
            return false;
        }
    }
    toString() {
        const this$ = this;
        return ("map [" + join("; ", map_1((kv) => format("({0}, {1})", kv[0], kv[1]), this$))) + "]";
    }
    get [Symbol.toStringTag]() {
        return "FSharpMap";
    }
    toJSON() {
        const this$ = this;
        return Array.from(this$);
    }
    GetEnumerator() {
        const _ = this;
        return MapTreeModule_mkIEnumerator(_.tree);
    }
    [Symbol.iterator]() {
        return toIterator(getEnumerator(this));
    }
    "System.Collections.IEnumerable.GetEnumerator"() {
        const _ = this;
        return MapTreeModule_mkIEnumerator(_.tree);
    }
    CompareTo(obj) {
        const m = this;
        if (obj instanceof FSharpMap) {
            const m2 = obj;
            return compareWith((kvp1, kvp2) => {
                const c = m.comparer.Compare(kvp1[0], kvp2[0]) | 0;
                return ((c !== 0) ? c : compare(kvp1[1], kvp2[1])) | 0;
            }, m, m2) | 0;
        }
        else {
            throw new Error("not comparable\\nParameter name: obj");
        }
    }
    "System.Collections.Generic.ICollection`1.Add2B595"(x) {
        throw new Error("Map cannot be mutated");
    }
    "System.Collections.Generic.ICollection`1.Clear"() {
        throw new Error("Map cannot be mutated");
    }
    "System.Collections.Generic.ICollection`1.Remove2B595"(x) {
        throw new Error("Map cannot be mutated");
    }
    "System.Collections.Generic.ICollection`1.Contains2B595"(x) {
        const m = this;
        return FSharpMap__ContainsKey(m, x[0]) && equals(FSharpMap__get_Item(m, x[0]), x[1]);
    }
    "System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"(arr, i) {
        const m = this;
        MapTreeModule_copyToArray(m.tree, arr, i);
    }
    "System.Collections.Generic.ICollection`1.get_IsReadOnly"() {
        return true;
    }
    "System.Collections.Generic.ICollection`1.get_Count"() {
        const m = this;
        return FSharpMap__get_Count(m) | 0;
    }
    "System.Collections.Generic.IReadOnlyCollection`1.get_Count"() {
        const m = this;
        return FSharpMap__get_Count(m) | 0;
    }
    get size() {
        const m = this;
        return FSharpMap__get_Count(m) | 0;
    }
    clear() {
        throw new Error("Map cannot be mutated");
    }
    delete(_arg) {
        throw new Error("Map cannot be mutated");
        return false;
    }
    entries() {
        const m = this;
        return map_1((p) => [p[0], p[1]], m);
    }
    get(k) {
        const m = this;
        return FSharpMap__get_Item(m, k);
    }
    has(k) {
        const m = this;
        return FSharpMap__ContainsKey(m, k);
    }
    keys() {
        const m = this;
        return map_1((p) => p[0], m);
    }
    set(k, v) {
        const m = this;
        throw new Error("Map cannot be mutated");
        return m;
    }
    values() {
        const m = this;
        return map_1((p) => p[1], m);
    }
    forEach(f, thisArg) {
        const m = this;
        iterate_1((p) => {
            f(p[1], p[0], m);
        }, m);
    }
}
export function FSharpMap_$reflection(gen0, gen1) {
    return class_type("Map.FSharpMap", [gen0, gen1], FSharpMap);
}
export function FSharpMap_$ctor(comparer, tree) {
    return new FSharpMap(comparer, tree);
}
export function FSharpMap_Empty(comparer) {
    return FSharpMap_$ctor(comparer, MapTreeModule_empty());
}
export function FSharpMap__get_Comparer(m) {
    return m.comparer;
}
export function FSharpMap__get_Tree(m) {
    return m.tree;
}
export function FSharpMap__Add(m, key, value) {
    return FSharpMap_$ctor(m.comparer, MapTreeModule_add(m.comparer, key, value, m.tree));
}
export function FSharpMap__Change(m, key, f) {
    return FSharpMap_$ctor(m.comparer, MapTreeModule_change(m.comparer, key, f, m.tree));
}
export function FSharpMap__get_IsEmpty(m) {
    return m.tree == null;
}
export function FSharpMap__get_Item(m, key) {
    return MapTreeModule_find(m.comparer, key, m.tree);
}
export function FSharpMap__TryPick(m, f) {
    return MapTreeModule_tryPick(f, m.tree);
}
export function FSharpMap__Exists(m, predicate) {
    return MapTreeModule_exists(predicate, m.tree);
}
export function FSharpMap__Filter(m, predicate) {
    return FSharpMap_$ctor(m.comparer, MapTreeModule_filter(m.comparer, predicate, m.tree));
}
export function FSharpMap__ForAll(m, predicate) {
    return MapTreeModule_forall(predicate, m.tree);
}
export function FSharpMap__Fold(m, f, acc) {
    return MapTreeModule_foldBack(f, m.tree, acc);
}
export function FSharpMap__FoldSection(m, lo, hi, f, acc) {
    return MapTreeModule_foldSection(m.comparer, lo, hi, f, m.tree, acc);
}
export function FSharpMap__Iterate(m, f) {
    MapTreeModule_iter(f, m.tree);
}
export function FSharpMap__MapRange(m, f) {
    return FSharpMap_$ctor(m.comparer, MapTreeModule_map(f, m.tree));
}
export function FSharpMap__Map(m, f) {
    return FSharpMap_$ctor(m.comparer, MapTreeModule_mapi(f, m.tree));
}
export function FSharpMap__Partition(m, predicate) {
    const patternInput = MapTreeModule_partition(m.comparer, predicate, m.tree);
    return [FSharpMap_$ctor(m.comparer, patternInput[0]), FSharpMap_$ctor(m.comparer, patternInput[1])];
}
export function FSharpMap__get_Count(m) {
    return MapTreeModule_size(m.tree);
}
export function FSharpMap__ContainsKey(m, key) {
    return MapTreeModule_mem(m.comparer, key, m.tree);
}
export function FSharpMap__Remove(m, key) {
    return FSharpMap_$ctor(m.comparer, MapTreeModule_remove(m.comparer, key, m.tree));
}
export function FSharpMap__TryGetValue(_, key, value) {
    const matchValue = MapTreeModule_tryFind(_.comparer, key, _.tree);
    if (matchValue == null) {
        return false;
    }
    else {
        const v = value_1(matchValue);
        value.contents = v;
        return true;
    }
}
export function FSharpMap__get_Keys(_) {
    return map_2((kvp) => kvp[0], MapTreeModule_toArray(_.tree));
}
export function FSharpMap__get_Values(_) {
    return map_2((kvp) => kvp[1], MapTreeModule_toArray(_.tree));
}
export function FSharpMap__get_MinKeyValue(m) {
    return MapTreeModule_leftmost(m.tree);
}
export function FSharpMap__get_MaxKeyValue(m) {
    return MapTreeModule_rightmost(m.tree);
}
export function FSharpMap__TryFind(m, key) {
    return MapTreeModule_tryFind(m.comparer, key, m.tree);
}
export function FSharpMap__ToList(m) {
    return MapTreeModule_toList(m.tree);
}
export function FSharpMap__ToArray(m) {
    return MapTreeModule_toArray(m.tree);
}
export function FSharpMap__ComputeHashCode(this$) {
    const combineHash = (x, y) => (((x << 1) + y) + 631);
    let res = 0;
    const enumerator = getEnumerator(this$);
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const activePatternResult = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            res = (combineHash(res, structuralHash(activePatternResult[0])) | 0);
            res = (combineHash(res, structuralHash(activePatternResult[1])) | 0);
        }
    }
    finally {
        disposeSafe(enumerator);
    }
    return res | 0;
}
export function isEmpty(table) {
    return FSharpMap__get_IsEmpty(table);
}
export function add(key, value, table) {
    return FSharpMap__Add(table, key, value);
}
export function change(key, f, table) {
    return FSharpMap__Change(table, key, f);
}
export function find(key, table) {
    return FSharpMap__get_Item(table, key);
}
export function tryFind(key, table) {
    return FSharpMap__TryFind(table, key);
}
export function remove(key, table) {
    return FSharpMap__Remove(table, key);
}
export function containsKey(key, table) {
    return FSharpMap__ContainsKey(table, key);
}
export function iterate(action, table) {
    FSharpMap__Iterate(table, action);
}
export function tryPick(chooser, table) {
    return FSharpMap__TryPick(table, chooser);
}
export function pick(chooser, table) {
    const matchValue = tryPick(chooser, table);
    if (matchValue != null) {
        return value_1(matchValue);
    }
    else {
        throw new Error();
    }
}
export function exists(predicate, table) {
    return FSharpMap__Exists(table, predicate);
}
export function filter(predicate, table) {
    return FSharpMap__Filter(table, predicate);
}
export function partition(predicate, table) {
    return FSharpMap__Partition(table, predicate);
}
export function forAll(predicate, table) {
    return FSharpMap__ForAll(table, predicate);
}
export function map(mapping, table) {
    return FSharpMap__Map(table, mapping);
}
export function fold(folder, state, table) {
    return MapTreeModule_fold(folder, state, FSharpMap__get_Tree(table));
}
export function foldBack(folder, table, state) {
    return MapTreeModule_foldBack(folder, FSharpMap__get_Tree(table), state);
}
export function toSeq(table) {
    return map_1((kvp) => [kvp[0], kvp[1]], table);
}
export function findKey(predicate, table) {
    return pick_1((kvp) => {
        const k = kvp[0];
        if (predicate(k, kvp[1])) {
            return some(k);
        }
        else {
            return void 0;
        }
    }, table);
}
export function tryFindKey(predicate, table) {
    return tryPick_1((kvp) => {
        const k = kvp[0];
        if (predicate(k, kvp[1])) {
            return some(k);
        }
        else {
            return void 0;
        }
    }, table);
}
export function ofList(elements, comparer) {
    return FSharpMap_$ctor(comparer, MapTreeModule_ofSeq(comparer, elements));
}
export function ofSeq(elements, comparer) {
    return FSharpMap_$ctor(comparer, MapTreeModule_ofSeq(comparer, elements));
}
export function ofArray(elements, comparer) {
    return FSharpMap_$ctor(comparer, MapTreeModule_ofSeq(comparer, elements));
}
export function toList(table) {
    return FSharpMap__ToList(table);
}
export function toArray(table) {
    return FSharpMap__ToArray(table);
}
export function keys(table) {
    return FSharpMap__get_Keys(table);
}
export function values(table) {
    return FSharpMap__get_Values(table);
}
export function minKeyValue(table) {
    return FSharpMap__get_MinKeyValue(table);
}
export function maxKeyValue(table) {
    return FSharpMap__get_MaxKeyValue(table);
}
export function empty(comparer) {
    return FSharpMap_Empty(comparer);
}
export function count(table) {
    return FSharpMap__get_Count(table);
}
