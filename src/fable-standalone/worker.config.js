module.exports = {
    target: "webworker",
    node: {
        fs: "empty",
        module: "empty",
        net: "empty",
    },
    mode: "production",
    // optimization: {
    //     minimize: false
    // }
};
