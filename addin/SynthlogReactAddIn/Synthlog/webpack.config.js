const devCerts = require("office-addin-dev-certs");
const CleanWebpackPlugin = require("clean-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const ExtractTextPlugin = require('extract-text-webpack-plugin');
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require('webpack');

module.exports = async (env, options)  => {
  const dev = options.mode === "development";
  const config = {
    devtool: "source-map",
    entry: {
    vendor: [
        'react',
        'react-dom',
        'core-js',
        'office-ui-fabric-react',
        'isomorphic-fetch'
    ],
    polyfill: 'babel-polyfill',
    taskpane: [
      'react-hot-loader/patch',
      './src/taskpane/index.js',
    ],
    menu: [
      'react-hot-loader/patch',
      './src/taskpane/menu.js',
    ],
    mercs: [
      'react-hot-loader/patch',
      './src/taskpane/mercs.js',
    ],
    tacle: [
      'react-hot-loader/patch',
      './src/taskpane/tacle.js',
    ],
    commands: './src/commands/commands.js'
    },
    resolve: {
      extensions: [".ts", ".tsx", ".html", ".js"]
    },
    module: {
      rules: [
        {
          test: /\.jsx?$/,
          use: [
              'react-hot-loader/webpack',
              'babel-loader',
          ],
          exclude: /node_modules/
        },
        {
          test: /\.css$/,
          use: ['style-loader', 'css-loader']
        },
        {
          test: /\.(png|jpe?g|gif|svg|woff|woff2|ttf|eot|ico)$/,
          use: {
              loader: 'file-loader',
              query: {
                  name: 'assets/[name].[ext]'
                }
              }  
            }   
          ]
    },    
    plugins: [
      new CleanWebpackPlugin(),
      new CopyWebpackPlugin([
        {
          to: "taskpane.css",
          from: "./src/taskpane/taskpane.css"
        }
      ]),
      new ExtractTextPlugin('[name].[hash].css'),
      new HtmlWebpackPlugin({
        filename: "menu.html",
        template: "./src/taskpane/menu.html",
        chunks: ['menu', 'vendor', 'polyfill']
      }),
      new HtmlWebpackPlugin({
        filename: "mercs.html",
        template: "./src/taskpane/mercs.html",
        chunks: ['mercs', 'vendor', 'polyfill']
      }),
      new HtmlWebpackPlugin({
        filename: "tacle.html",
        template: "./src/taskpane/tacle.html",
        chunks: ['tacle', 'vendor', 'polyfill']
      }),
      new HtmlWebpackPlugin({
        filename: "taskpane.html",
          template: './src/taskpane/taskpane.html',
          chunks: ['taskpane', 'vendor', 'polyfill']
      }),
      new HtmlWebpackPlugin({
          filename: "commands.html",
          template: "./src/commands/commands.html",
          chunks: ["commands"]
      }),
      new CopyWebpackPlugin([
          {
              from: './assets',
              ignore: ['*.scss'],
              to: 'assets',
          }
      ]),
      new webpack.ProvidePlugin({
        Promise: ["es6-promise", "Promise"]
      })
    ],
    devServer: {
      headers: {
        "Access-Control-Allow-Origin": "*"
      },
      https: await devCerts.getHttpsServerOptions(),
      port: 3000
    }
  };

  return config;
};
