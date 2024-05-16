import React, { useState } from "react";
import { useNavigate } from "react-router-dom";

import { Modal } from "../../components/modal/modal";
import { useGameParams } from "../../context";
import { useParams } from "../../hooks";

import { players } from "../../data/player";
import type { param } from "../../types/params";

import "./home.css";

function Home() {
    const [showSettingsModal, setShowSettingsModal] = useState(false);
    const p = useGameParams();
    const navigate = useNavigate();
    let { params, setParam }: any = useParams();

    if (p) {
        ({ params, setParam } = p);
    }

    const handleModalDisplay = () => {
        setShowSettingsModal(!showSettingsModal);
    }

    const handleStartGame = () => {
        console.log("params 1", params);
        for (let i = 0; i < params.length; i++) {
            setParam(i, true);
        }
        console.log("params 2", params);
        navigate("/game");
    }

    return (
        <>
            {showSettingsModal && <Modal >
                <h2>Paramètre de la partie</h2>
                {params.map((parameter: param, index: number) => (
                    <div key={index} className="player-settings">
                        <div>{players[index].teamName}</div>
                        <label>
                            <input type="checkbox" checked={parameter.isHuman} onChange={(e) => setParam(index, e.target.checked)} />
                            Humain
                        </label>
                    </div>
                ))}
                <div>
                    <button className="modal-button" onClick={handleStartGame}>Commencer</button>
                    <button className="modal-button" onClick={handleModalDisplay}>Retour</button>
                </div>
            </Modal>}
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
                    <p>La partie finie quand tous joueurs ont passé la ligne d'arrivée</p>
                </div>
                <div className="start-game">
                    <p>Cliquer sur "nouvelle partie" pour accèder au parametre de la partie</p>
                    <button className={`modal-button ${showSettingsModal ? "active": ""}`} onClick={handleModalDisplay}>Nouvelle partie</button>
                </div>
            </div>
        </>
    );
}

export default Home;