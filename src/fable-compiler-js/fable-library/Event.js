import { subscribe } from "./Observable.js";
import { some, value } from "./Option.js";
import { Choice_tryValueIfChoice1Of2, Choice_tryValueIfChoice2Of2 } from "./Choice.js";
export class Event$2 {
    constructor() {
        this.delegates = [];
    }
    _add(d) {
        this.delegates.push(d);
    }
    _remove(d) {
        const index = this.delegates.indexOf(d);
        if (index > -1) {
            this.delegates.splice(index, 1);
        }
    }
    get Publish() {
        return createEvent(h => { this._add(h); }, h => { this._remove(h); });
    }
    Trigger(senderOrValue, valueOrUndefined) {
        let sender = null;
        const value = valueOrUndefined === undefined ? senderOrValue : (sender = senderOrValue, valueOrUndefined);
        this.delegates.forEach(f => { f(sender, value); });
    }
}
export class Event extends Event$2 {
}
export function add(callback, sourceEvent) {
    subscribe(callback, sourceEvent);
}
export function choose(chooser, sourceEvent) {
    const ev = new Event();
    add((t) => {
        const u = chooser(t);
        if (u != null) {
            ev.Trigger(value(u));
        }
    }, sourceEvent);
    return ev.Publish;
}
export function filter(predicate, sourceEvent) {
    return choose((x) => predicate(x) ? some(x) : undefined, sourceEvent);
}
export function map(mapping, sourceEvent) {
    const ev = new Event();
    add((t) => ev.Trigger(mapping(t)), sourceEvent);
    return ev.Publish;
}
export function merge(event1, event2) {
    const ev = new Event();
    const fn = (x) => ev.Trigger(x);
    add(fn, event1);
    add(fn, event2);
    return ev.Publish;
}
export function pairwise(sourceEvent) {
    const ev = new Event();
    let last;
    let haveLast = false;
    add((next) => {
        if (haveLast) {
            ev.Trigger([last, next]);
        }
        last = next;
        haveLast = true;
    }, sourceEvent);
    return ev.Publish;
}
export function partition(predicate, sourceEvent) {
    return [filter(predicate, sourceEvent), filter((x) => !predicate(x), sourceEvent)];
}
export function scan(collector, state, sourceEvent) {
    return map((t) => state = collector(state, t), sourceEvent);
}
export function split(splitter, sourceEvent) {
    return [
        choose((v) => Choice_tryValueIfChoice1Of2(splitter(v)), sourceEvent),
        choose((v) => Choice_tryValueIfChoice2Of2(splitter(v)), sourceEvent),
    ];
}
export function createEvent(addHandler, removeHandler) {
    return {
        AddHandler(h) { addHandler(h); },
        RemoveHandler(h) { removeHandler(h); },
        Subscribe(r) {
            const h = ((_, args) => r.OnNext(args));
            addHandler(h);
            return {
                Dispose() { removeHandler(h); }
            };
        }
    };
}
