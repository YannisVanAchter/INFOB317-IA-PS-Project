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

### Bot

.... TODO ....

### AI 

.... TODO ....

## Run App in Production

Ensure [Docker](https://www.docker.com/) is correctly installed on your computer. 

Use the command:
```bash
$ docker-compose up -d
```

 ## Exemple websocket

- Open a terminal and navigate to the directory ./example/example_websocket_bot/
- Run the command =swipl echo-ws-server.pl=
- Execute =:- start_server=
- Open your browser and enter http://localhost:3000 in the address bar
- the /echo route is for WebSocket communication