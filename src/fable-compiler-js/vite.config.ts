import path from 'path';
import { defineConfig } from 'vite';

export default defineConfig({
    build: {
        lib: {
            entry: 'temp/app.js',
            name: 'fable-compiler-js',
            fileName: (format) => `fable-compiler-js.${format}.js`,
        },
        rollupOptions: {
          external: [
            "url.fileURLToPath(new URL('.', import.meta.url))"
          ],
        }
    },
})
