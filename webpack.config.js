const path = require("path");
module.exports = {
	mode: "production",
	entry: "./pages/index.js",
	output: {
		path: path.resolve(__dirname, "docs"),
		filename: "bundle.js",
	},
	devServer: {
		contentBase: path.resolve(__dirname, "docs"),
		watchContentBase: true,
	},
};
