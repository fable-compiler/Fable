/// @ts-check

function fetchBlob(getUrl, name) {
    return fetch(getUrl(name))
        .then(function (res) {
            if (res.ok) {
                return res.arrayBuffer().then(b => {
                    return [name, new Uint8Array(b)]
                });
            } else {
                throw new Error("[ASSEMBLY LOAD] " + res.status + ": " + res.statusText);
            }
        });
}

export function getAssemblyReader(getUrl, assemblies) {
    return Promise.all(assemblies.map(name => fetchBlob(getUrl, name)))
        .then(function (kvs) {
            var metadata = new Map();
            for (var kv of kvs) {
                metadata.set(kv[0] + ".dll", kv[1]);
            }
            return (name) => metadata.get(name);
        });
}

// JSON.stringify doesn't escape line/paragraph separators, which are not valid in JS.
// https://github.com/expressjs/express/issues/1132
export function escapeJsStringLiteral(str) {
  return JSON.stringify(str).slice(1, -1).replace(/\u2028/g, '\\u2028').replace(/\u2029/g, '\\u2029');
}
