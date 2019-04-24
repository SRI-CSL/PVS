({
	optimize: "none",
	baseUrl: "../src/client/app",
 	paths: {
        "d3": "../lib/d3",
        "pvsioweb": "prototypebuilder",
        "imagemapper": "../lib/imagemapper",
        "text": "../lib/text",
        "lib": "../lib",
		"almond": "../../../dist/almond"
    },
	wrap: {
		startFile: "./start.frag",
		endFile: "./end.frag"
	},
	out: "pvsioweb-client.js",
	include: ["PVSioWebClient"],
	name: "almond"
})