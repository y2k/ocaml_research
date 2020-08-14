const nodeResolve = require(`rollup-plugin-node-resolve`);
const pnpResolve = require(`rollup-plugin-pnp-resolve`);

export default {
  input: 'material-dependencies',
  output: {
    file: 'output/bundle.js',
    format: 'cjs'
  },
  plugins: [
    nodeResolve(),
    pnpResolve()
  ]
};
