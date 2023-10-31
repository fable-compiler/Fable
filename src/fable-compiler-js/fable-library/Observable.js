import { Choice_tryValueIfChoice1Of2, Choice_tryValueIfChoice2Of2 } from "./Choice.js";
import { value } from "./Option.js";
export class Observer {
    constructor(onNext, onError, onCompleted) {
        this.OnNext = onNext;
        this.OnError = onError || ((_e) => { return; });
        this.OnCompleted = onCompleted || (() => { return; });
    }
}
class Observable {
    constructor(subscribe) {
        this.Subscribe = subscribe;
    }
}
function protect(f, succeed, fail) {
    try {
        return succeed(f());
    }
    catch (e) {
        fail(e);
    }
}
export function add(callback, source) {
    source.Subscribe(new Observer(callback));
}
export function choose(chooser, source) {
    return new Observable((observer) => source.Subscribe(new Observer((t) => protect(() => chooser(t), (u) => { if (u != null) {
        observer.OnNext(value(u));
    } }, observer.OnError), observer.OnError, observer.OnCompleted)));
}
export function filter(predicate, source) {
    return choose((x) => predicate(x) ? x : void 0, source);
}
export function map(mapping, source) {
    return new Observable((observer) => source.Subscribe(new Observer((t) => {
        protect(() => mapping(t), observer.OnNext, observer.OnError);
    }, observer.OnError, observer.OnCompleted)));
}
export function merge(source1, source2) {
    return new Observable((observer) => {
        let stopped = false;
        let completed1 = false;
        let completed2 = false;
        const h1 = source1.Subscribe(new Observer((v) => { if (!stopped) {
            observer.OnNext(v);
        } }, (e) => {
            if (!stopped) {
                stopped = true;
                observer.OnError(e);
            }
        }, () => {
            if (!stopped) {
                completed1 = true;
                if (completed2) {
                    stopped = true;
                    observer.OnCompleted();
                }
            }
        }));
        const h2 = source2.Subscribe(new Observer((v) => { if (!stopped) {
            observer.OnNext(v);
        } }, (e) => {
            if (!stopped) {
                stopped = true;
                observer.OnError(e);
            }
        }, () => {
            if (!stopped) {
                completed2 = true;
                if (completed1) {
                    stopped = true;
                    observer.OnCompleted();
                }
            }
        }));
        return {
            Dispose() {
                h1.Dispose();
                h2.Dispose();
            },
        };
    });
}
export function pairwise(source) {
    return new Observable((observer) => {
        let last;
        return source.Subscribe(new Observer((next) => {
            if (last != null) {
                observer.OnNext([last, next]);
            }
            last = next;
        }, observer.OnError, observer.OnCompleted));
    });
}
export function partition(predicate, source) {
    return [filter(predicate, source), filter((x) => !predicate(x), source)];
}
export function scan(collector, state, source) {
    return new Observable((observer) => {
        return source.Subscribe(new Observer((t) => {
            protect(() => collector(state, t), (u) => { state = u; observer.OnNext(u); }, observer.OnError);
        }, observer.OnError, observer.OnCompleted));
    });
}
export function split(splitter, source) {
    return [
        choose((v) => Choice_tryValueIfChoice1Of2(splitter(v)), source),
        choose((v) => Choice_tryValueIfChoice2Of2(splitter(v)), source)
    ];
}
export function subscribe(callback, source) {
    return source.Subscribe(new Observer(callback));
}
