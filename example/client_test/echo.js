const WS_PROTO = "ws://" //Protocole utilisé pour la connexion websocket
const WS_ROUTE = "/echo" //Spécifier le chemin vers le serv WebSocket

function log(topic, message) { //fonction pour afficher les messages dans la console
  console.log('[' + topic + '] ' + message)
}

function wsMessageHandler(event) { //la fct qui s'occupe de la reception des messages par le serveur, c'est surtout pour tester mais faudra surement modifier dans notre cas
  const payload = JSON.parse(event.data) //Converti le message reçu en object JavaScript
  log("WS Response", "Received message: '" + event.data + "'") //utiliser la fct log

  const messages = document.getElementById("messages") //Séléctionner l'élément HTML où les messages doivent être affichés
  const message = document.createElement("div") //Créer un nouveau div pour contenir le message
  message.className = 'message' //Assigner une classe CSS au div

  const contentElement = document.createElement("div") //Créer un nouveau div pour contenir le contenu du message
  contentElement.className = 'content' //Assigner une classe CSS au div
  contentElement.appendChild(document.createTextNode(payload.message)) //crée un "nœud de texte" contenant le contenu du message (venant de payload) et l'ajoute à l'élément <div> créé pour le contenu du message

  message.appendChild(contentElement) //add du contenu du message au div
  let child = messages.appendChild(message) //Add le message div au container de messages

  child.scrollIntoView() //Faire défiler la page pour afficher le nouveau message
}
function sendMessage(connection, message) { //fonction pour envoyer des messages au serveur via le websocket (donc ici c'est l'input)
  log("Client", "sending message \"" + message + "\"")
  connection.send(message)
}

function openWebSocket() { //fonction pour ouvrir une connexion websocket
  connection = new WebSocket(WS_PROTO + window.location.host + WS_ROUTE) //crée une nouveau WebSocket avec l'url (se basant sur le protocole, l'hôte et le chemin spécifiés)
  connection.onerror = (error) => { //le handler pour les erreurs qui se produisent lors de la connexion websocket
    log("WS", error)
  } 
  connection.onmessage = wsMessageHandler //le handler pour les messages reçus via la connexion websocket, donc on désigne ici où les messages reçus doivent être traités
  return connection
}

document.addEventListener('DOMContentLoaded', (e) => { //fonction qui s'exécute lorsque le document HTML est chargé
  const input_box = document.getElementById("input-message") //Sélectionne la zone de texte où l'utilisateur peut entrer des messages
  const input_button = document.getElementById("message-submit") //Sélectionne le bouton qui envoie le message
  const connection = openWebSocket() //ouvre la connexion websocket et stocke l'objet de connexion dans une variable
  input_button.addEventListener("click", (event) => { //add un event listener pour le click sur le bouton 
    const isBot = false; //Ici faut changer manuellement pour le moment donc faudra surement modifier. 
    //le but c'est d'avoir deux id différents pour si c'est une requete pour le bot ou l'ia
    const payload = { //Crée un plyload object contenant le message entré dans la zone de texte
      id: isBot ? "bot" : "ia", //si isBot = true, alors id = "bot", sinon id = "ia"
      content: input_box.value
    } 
    sendMessage(connection, JSON.stringify(payload)) //Envoie le message au serveur via la connexion websocket, converti en JSON.
  })
  log("OnLoad", "Add event listeners") //Log
})
