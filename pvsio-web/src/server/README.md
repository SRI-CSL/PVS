#Server
This directory contains code that gets executed using nodejs.

###pvssocketserver.js
This is the entry point for the server application. It creates a websocket/http server. The http server serves static content from the `../client/` directory. The http server also serves the `public/demos`  - in order to allow users to browse demos. The websocket server enables the creation and management of projects in the `public/projects` directory. It also provides a channel of communication between clients and the pvs process.