// This file is just a reference to locate the `lib` directory

import { dirname } from 'dirname-filename-esm';
import path from 'path';

const currentFileDir = dirname(import.meta);

export function getAssembliesDir () {
    return path.join(currentFileDir, "lib");
}
