import React from "react";

import { GameSettingsModal } from "../../components/gameSettingsModal/gameSettingsModal";

import "./home.css";

function Home() {
    const [showSettingsModal, setShowSettingsModal] = React.useState(false);
    return (
        <div className="page home">
            <div className="rules">
                <h1 className="title">Règles</h1>
                <p>Le jeu se joue à 4 joueurs</p>
                <p>Le jeu se joue avec 3 vélo par coureur</p>
                <p>Le but du jeu est d'avoir un temps total le plus petit possible</p>
                <p>Chaque joueur a 5 carte maximum, et repioche quand il n'en a plus</p>
                <p>Il y a des cases "chance" qui donne un bonus ou malus allant de -3 à +3</p>
                <p>Les vélo de chaque joueurs sont joué à tous les tours. Le premier vélo joué étant le premier sur le plateau, suivi du suivant etc</p>
                <p>Un système d'aspiration permet d'aller plus loin avec la meme carte chance</p>
                <p>Le premier joueur à jouer est celui avec la carte seconde la plus élevée</p>
                <p>La partie finie quand un joueur passe la ligne d'arrivée, on fini alors le tour en cours</p>
            </div>
            <div className="start-game">
                <p>Cliquer sur "nouvelle partie" pour accèder au parametre de la partie</p>
                <button className={`modal-button ${showSettingsModal ? "active": ""}`} onClick={() => setShowSettingsModal(true)}>Nouvelle partie</button>
            </div>
            {showSettingsModal && <GameSettingsModal modalSettingsDisplay={{showSettingsModal, setShowSettingsModal}} />}
        </div>
    );
}

export default Home;