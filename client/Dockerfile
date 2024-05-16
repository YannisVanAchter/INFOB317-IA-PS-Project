# Use an official Node.js runtime as the base image
FROM node:14-alpine

# Set the working directory in the container
WORKDIR /app

# Copy package.json and package-lock.json to the working directory
COPY package*.json ./

# Install app dependencies
RUN npm install

# Copy the rest of the app source code to the working directory
COPY ./public ./public
COPY ./src ./src
COPY ./custom.d.ts ./custom.d.ts
COPY ./tsconfig.json ./tsconfig.json

# Define the command to run the app
CMD ["npm", "start"]
