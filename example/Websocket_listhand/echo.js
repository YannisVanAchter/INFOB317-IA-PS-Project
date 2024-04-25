const WS_PROTO = "ws://" //"wss://" for secure websockets
const WS_ROUTE = "/echo" //Route to the websocket server

//Log function to print messages to the console
function log(topic, message) {
  console.log('[' + topic + '] ' + message)
}

//Function to handle incoming messages from the websocket (server -> (websocket) -> client)
function wsMessageHandler(event) {
  const payload = JSON.parse(event.data); 
  log("WS Response", "Received message: '" + event.data + "'");

  const messageReceived = document.getElementById("message-received"); //Here, we are selecting the div element with the id "message-received" where we will display the received messages
  
  if (payload.hasOwnProperty("randomValue")) { //Checks if the payload contains a "randomValue" property
    const randomValue = payload.randomValue;
    const messageElement = document.createElement("div"); //Creates a new div for the message
    messageElement.textContent = "Random value chosen: " + randomValue; //Adds the message content to the div
    messageReceived.appendChild(messageElement); //Adds the div to the messages container
  } else { //If the payload does not contain a "randomValue", simply display the message
    const messageElement = document.createElement("div"); //Creates a new div for the message
    messageElement.textContent = payload.message; //Adds the message content to the div
    messageReceived.appendChild(messageElement); //Adds the div to the messages container
  }
}

//Function to send messages to the websocket (client -> (websocket) -> server)
function sendMessage(connection, message) {
  log("Client", "sending message \"" + message + "\"")
  connection.send(message)
}

//Function to open a websocket connection to the server
function openWebSocket() {
  connection = new WebSocket(WS_PROTO + window.location.host + WS_ROUTE) //Create a new instance of WebSocket using the URL constructed with the specified protocol, host, and path
  //Event handler for errors that occur during the WebSocket connection
  connection.onerror = (error) => { 
    log("WS", error)
  }
  connection.onmessage = wsMessageHandler //Event handler for messages received via the WebSocket connection
  return connection
}

document.addEventListener('DOMContentLoaded', (e) => {
  const input_box = document.getElementById("input-message"); //Selects the input box where the user can type messages
  const input_button = document.getElementById("message-submit"); //Selects the button that the user can click to send messages
  const connection = openWebSocket(); //Opens a WebSocket connection to the server

  input_button.addEventListener("click", (event) => { //Adds an event listener for click on the message submit button

    const inputString = input_box.value.trim(); //Retrieves the string entered in the input box and splits it into numbers separated by commas
    const numbers = inputString.split(',').map(Number); //Converts each element to a number

    sendMessage(connection, JSON.stringify(numbers)); //Sends the numbers to the server via the WebSocket connection, converting them to JSON string
  });

  log("OnLoad", "Add event listeners");
});