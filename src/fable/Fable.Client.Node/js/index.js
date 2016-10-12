#!/usr/bin/env node

var fableMain = require("./fable");

if (require.main === module) {
    fableMain.init();
} else {
    console.log('TODO: API for Fable as a service');
}
