/// <reference path="node_modules/@types/node/index.d.ts" />

import { List } from 'fable-core';

import * as Util from './util';

if (process.argv.length <= 2)
    console.log("Please provide an argument");
else {
    const name = process.argv[2];
    Util.greet(Util.reverse(name));

    // Use fable-core to create a List
    const list = List.ofArray([1, 2, 3, 4, 5]);
    
    const result = Util.sum(list);
    console.log(`sum([1, 2, 3, 4, 5]) = ${result}`);
}
