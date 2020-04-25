import resolve from '@rollup/plugin-node-resolve';

export default {
  input: 'material-dependencies',
  output: {
    file: 'output/bundle.js',
    format: 'cjs'
  },
  plugins: [resolve()]
};
