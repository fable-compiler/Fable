const util = require("../common");

util.runTargets({
    async test() {
        await util.runNpmBin("ava", "tests/index.js");
    },
    async build() {
        await util.runNpmBin("tslint", "--project .");
        await util.runNpmBin("tsc");
    }
});
