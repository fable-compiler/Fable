
export class ConditionalWeakTable<TKey extends object, TValue> {
  private weakMap: WeakMap<TKey, TValue> = new WeakMap<TKey, TValue>();

  public delete(key: TKey) {
    return this.weakMap.delete(key);
  }

  public get(key: TKey) {
    return this.weakMap.get(key);
  }

  public has(key: TKey) {
    return this.weakMap.has(key);
  }

  public set(key: TKey, value: TValue) {
    return this.weakMap.set(key, value);
  }

  public clear() {
    this.weakMap = new WeakMap<TKey, TValue>();
  }
}

export default ConditionalWeakTable;
