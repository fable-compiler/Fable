import { equals, isArrayLike } from "./Util.js";

export function count<T>(col: Iterable<T>): number {
  if (typeof (col as any)["System.Collections.Generic.ICollection`1.get_Count"] === "function") {
    return (col as any)["System.Collections.Generic.ICollection`1.get_Count"](); // collection
  } else {
    if (typeof (col as any)["System.Collections.Generic.IReadOnlyCollection`1.get_Count"] === "function") {
      return (col as any)["System.Collections.Generic.IReadOnlyCollection`1.get_Count"](); // collection
    } else {
      if (isArrayLike(col)) {
        return col.length; // array, resize array
      } else {
        if (typeof (col as any).size === "number") {
          return (col as any).size; // map, set
        } else {
          let count = 0;
          for (const _ of col) {
            count++;
          }
          return count; // other collections
        }
      }
    }
  }
}

export function isReadOnly<T>(col: Iterable<T>): boolean {
  if (typeof (col as any)["System.Collections.Generic.ICollection`1.get_IsReadOnly"] === "function") {
    return (col as any)["System.Collections.Generic.ICollection`1.get_IsReadOnly"](); // collection
  } else {
    if (isArrayLike(col)) {
      return ArrayBuffer.isView(col); // true for typed arrays, false for other arrays
    } else {
      if (typeof (col as any).size === "number") {
        return false; // map, set
      } else {
        return true; // other collections
      }
    }

  }
}

export function copyTo<T>(col: Iterable<T>, array: T[], arrayIndex: number) {
  if (typeof (col as any)["System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"] === "function") {
    (col as any)["System.Collections.Generic.ICollection`1.CopyToZ3B4C077E"](array, arrayIndex); // collection
  } else {
    let i = arrayIndex;
    for (const v of col) {
      array[i] = v;
      i++;
    }
  }
}

export function contains<T>(col: Iterable<T>, item: T): boolean {
  if (typeof (col as any)["System.Collections.Generic.ICollection`1.Contains2B595"] === "function") {
    return (col as any)["System.Collections.Generic.ICollection`1.Contains2B595"](item); // collection
  } else {
    if (isArrayLike(col)) {
      let i = col.findIndex(x => equals(x, item)); // array, resize array
      return i >= 0;
    } else {
      if (typeof (col as any).has === "function") {
        if (typeof (col as any).set === "function" && isArrayLike(item)) {
          return (col as any).has(item[0]) && equals((col as any).get(item[0]), item[1]); // map
        } else {
          return (col as any).has(item); // set
        }
      } else {
        return false; // other collections
      }
    }
  }
}

export function add<T>(col: Iterable<T>, item: T): void {
  if (typeof (col as any)["System.Collections.Generic.ICollection`1.Add2B595"] === "function") {
    return (col as any)["System.Collections.Generic.ICollection`1.Add2B595"](item); // collection
  } else {
    if (isArrayLike(col)) {
      if (ArrayBuffer.isView(col)) {
        // TODO: throw for typed arrays?
      } else {
        col.push(item); // array, resize array
      }
    } else {
      if (typeof (col as any).add === "function") {
        return (col as any).add(item); // set
      } else {
        if (typeof (col as any).has === "function"
          && typeof (col as any).set === "function"
          && isArrayLike(item)) {
          if ((col as any).has(item[0]) === false) {
            (col as any).set(item[0], item[1]); // map
          } else {
            throw new Error("An item with the same key has already been added. Key: " + item[0]);
          }
        } else {
          // TODO: throw for other collections?
        }
      }
    }
  }
}

export function remove<T>(col: Iterable<T>, item: T): boolean {
  if (typeof (col as any)["System.Collections.Generic.ICollection`1.Remove2B595"] === "function") {
    return (col as any)["System.Collections.Generic.ICollection`1.Remove2B595"](item); // collection
  } else {
    if (isArrayLike(col)) {
      if (ArrayBuffer.isView(col)) {
        // TODO: throw for typed arrays
        return false;
      } else {
        let i = col.findIndex(x => equals(x, item));
        if (i >= 0) {
          col.splice(i, 1); // array, resize array
          return true;
        } else {
          return false;
        }
      }
    } else {
      if (typeof (col as any).delete === "function") {
        if (typeof (col as any).set === "function" && isArrayLike(item)) {
          if ((col as any).has(item[0]) && equals((col as any).get(item[0]), item[1])) {
            return (col as any).delete(item[0]); // map
          } else {
            return false;
          }
        } else {
          return (col as any).delete(item); // set
        }
      } else {
        // TODO: throw for other collections?
        return false; // other collections
      }
    }
  }
}

export function clear<T>(col: Iterable<T>): void {
  if (typeof (col as any)["System.Collections.Generic.ICollection`1.Clear"] === "function") {
    return (col as any)["System.Collections.Generic.ICollection`1.Clear"](); // collection
  } else {
    if (isArrayLike(col)) {
      if (ArrayBuffer.isView(col)) {
        // TODO: throw for typed arrays?
      } else {
        col.splice(0); // array, resize array
      }
    } else {
      if (typeof (col as any).clear === "function") {
        (col as any).clear(); // map, set
      } else {
        // TODO: throw for other collections?
      }
    }
  }
}