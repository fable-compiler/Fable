const util = require("../common");

util.runTargets({
    async test() {
        await util.runNpmBin("mocha", "tests/index.js -t 30000");
    },
    async build() {
        await util.runNpmBin("tslint", "--project .");
        await util.runNpmBin("tsc");
    }
});
