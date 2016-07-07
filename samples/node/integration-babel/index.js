import * as Util from './util'

if (process.argv.length <= 2)
    console.log("Please provide an argument");
else {
    const name = process.argv[2];
    Util.greet(Util.reverse(name));
}
