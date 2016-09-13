var path = require("path");
var webpack = require("webpack");

module.exports = {
    devtool: "source-map",
    entry: [
        "webpack-dev-server/client?http://localhost:8080",
        'webpack/hot/only-dev-server',
        "./temp/index"
    ],
    output: "./bundle.js",
    module: {
        preLoaders: [{
            test: /\.js$/,
            exclude: /node_modules/,
            loader: "source-map-loader"
        }],
        loaders: [{
            test: /\.js$/,
            exclude: /node_modules/,
            loader: "react-hot-loader"
        }]
    },
    externals: {
        'Redux': true
    },
    plugins: [
        new webpack.HotModuleReplacementPlugin()    
    ],
    devServer: {
        hot: true,
        contentBase: "./",
        publicPath: "/",
        historyApiFallback: true
    }
};
