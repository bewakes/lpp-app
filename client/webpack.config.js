const path = require('path');
const webpack = require('webpack');

const HtmlWebpackPlugin = require('html-webpack-plugin');


module.exports = {
    entry: ['@babel/polyfill/noConflict', './src/index'],
    output: {
        path: path.join(path.dirname(__dirname), '/dist'),
        filename: 'bundle.js',
    },

    resolve: {
        extensions: ['.ts', '.tsx', '.js']
    },

    devServer: {
        disableHostCheck: true,
        // contentBase: path.join(path.dirname(__dirname), '/src'),
        historyApiFallback: true,
        hot: true,
        overlay: {
            errors: true,
            warnings: true,
        },
    },

    module: {
        rules: [
            {
                test: /\.(ts|js)x?$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader'
                },
            },
            // css loader
            {
                test: /\.(css|scss)$/,
                use: ['style-loader', 'css-loader', 'sass-loader']
            }
        ]
    },

    plugins: [
        new HtmlWebpackPlugin({
          template: './src/index.html'
        })
    ]
};
