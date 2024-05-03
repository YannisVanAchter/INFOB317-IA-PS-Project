declare module "*/*.svg" {
    const content: string;
    export default content;
}

const webpack = require('webpack');

module.exports = {
    entry: './src/index.tsx',
    output: {
        filename: 'bundle.js',
        path: __dirname + '/dist',
    },
    resolve: {
        extensions: ['.ts', '.tsx', '.js', '.svg'],
    },
    module: {
        rules: [
            { test: /\.tsx?$/, loader: 'ts-loader' },
            { test: /\.svg$/, loader: 'svg-inline-loader' },
            { test: /\.svg$/, use: ['@svgr/webpack'] },
        ],
    },
    plugins: [
        new webpack.DefinePlugin({
            'process.env.REACT_DEBUG': JSON.stringify(process.env.REACT_DEBUG),
        }),
    ],
};