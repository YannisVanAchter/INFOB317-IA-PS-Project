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

  const messageReceived = document.getElementById("message-received"); 

  
  if (payload.hasOwnProperty("randomValue")) {
    const randomValue = payload.randomValue;
    const messageElement = document.createElement("div"); 
    messageElement.textContent = "Valeur aléatoire choisie: " + randomValue; 
    messageReceived.appendChild(messageElement); 
  } else {
   
    const messageElement = document.createElement("div"); 
    messageElement.textContent = payload.message; 
    messageReceived.appendChild(messageElement); 
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
  const input_box = document.getElementById("input-message"); 
  const input_button = document.getElementById("message-submit"); 
  const connection = openWebSocket(); 

  input_button.addEventListener("click", (event) => {

    const inputString = input_box.value.trim(); 
    const numbers = inputString.split(',').map(Number); 

    sendMessage(connection, JSON.stringify(numbers));
  });

  log("OnLoad", "Add event listeners");
});