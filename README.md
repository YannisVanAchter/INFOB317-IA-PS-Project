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
