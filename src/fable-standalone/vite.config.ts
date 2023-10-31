import path from 'path';
import { defineConfig } from 'vite';

export default defineConfig({
    build: {
        lib: {
            entry: 'temp/Main.js',
            name: 'FABLE_STANDALONE',
            fileName: (format) => `FABLE_STANDALONE.${format}.js`
        }
    }
})
