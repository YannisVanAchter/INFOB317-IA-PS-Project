# INFOB317-IA---PS-Project

## Run App in Development Mode

Use 3 terminals.

### Client

Ensure the file [.env](./client/.env) exists before executing commands.

If it does not exist, copy the content of [exemple.env](./client/exemple.env) into the newly created `.env`.

In the [client](./client/) directory, use:

```bash
$ npm install
$ npm start
```

The first command installs dependencies (React, TypeScript, and Boardgames.io).

The second command starts the app on [localhost](http://localhost:3000/).

### Bot and AI 

To run the bot and the AI you will need to start a prolog server in [`server`](./server/socket.pl) 

To do it, run the following command in a terminal:

```bash
server $ swipl socket.pl
```

A short version on the API documentation is provide on the [home page](http://localhost:8080/home).