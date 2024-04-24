const WS_PROTO = "ws://" //"wss://" for secure websockets
const WS_ROUTE = "/echo" //Route to the websocket server

//Log function to print messages to the console
function log(topic, message) {
  console.log('[' + topic + '] ' + message)
}

//Function to handle incoming messages from the websocket (server -> (websocket) -> client)
function wsMessageHandler(event) {
  const payload = JSON.parse(event.data) //Converts the received JSON string into a JavaScript object
  log("WS Response", "Received message: '" + event.data + "'") //Logs the received message to the console

  const messages = document.getElementById("messages") //Selects the existing HTML element where messages are to be displayed
  const message = document.createElement("div") //Creates a new div to contain the received message
  message.className = 'message' //Assigns a CSS class to the div to style it

  const contentElement = document.createElement("div") //Creates a new div to contain the message content (text)
  contentElement.className = 'content' 
  contentElement.appendChild(document.createTextNode(payload.message)) //Adds the message content to the div. payload.message is assumed to be a property of the payload object containing the message sent by the server
  const timestampElement = document.createElement("div")
  timestampElement.className = 'timestamp'
  timestampElement.appendChild(document.createTextNode(new Date(payload.time*1000)))
  message.appendChild(timestampElement) 
  message.appendChild(contentElement) //Adds content elements to the main message div
  let child = messages.appendChild(message) //Adds the message div to the messages container

  child.scrollIntoView() //Scrolls the message container to the bottom to show the latest message
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
  const input_box = document.getElementById("input-message") //Selects the input box where the user can type messages
  const input_button = document.getElementById("message-submit") //Selects the button that the user can click to send messages
  const connection = openWebSocket() //Opens a WebSocket connection to the server
  //Add an event listener for click on the message submit button
  input_button.addEventListener("click", (event) => {
    //Create a payload object containing the message entered in the input box
    const payload = {
      message: input_box.value
    }
    sendMessage(connection, JSON.stringify(payload)) //Send the message to the server via the WebSocket connection, converting it to JSON string.
  })
  log("OnLoad", "Add event listeners") //Log the page load and the addition of event listeners
})
