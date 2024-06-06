import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { Modal } from '../../components/modal/modal';
import { useGameParams } from '../../context/gameSettingsContext';
import { players } from '../../data/player';
import './home.css';

const Home: React.FC = () => {
    const [showSettingsModal, setShowSettingsModal] = useState(false);
    const { params, setParam } = useGameParams();
    const navigate = useNavigate();

    const handleModalDisplay = () => {
        setShowSettingsModal(!showSettingsModal);
    };

    const handleStartGame = () => {
        navigate('/game');
    };

    return (
        <>
            {showSettingsModal && (
                <Modal>
                    <h2>Paramètre de la partie</h2>
                    {params.map((parameter, index) => (
                        <div key={index} className="player-settings">
                            <div>{players[index].teamName}</div>
                            <label>
                                <input
                                    type="checkbox"
                                    checked={parameter.isHuman}
                                    onChange={(e) => setParam(index, e.target.checked)}
                                />
                                Humain
                            </label>
                        </div>
                    ))}
                    <div>
                        <button className="modal-button" onClick={handleStartGame}>Commencer</button>
                        <button className="modal-button" onClick={handleModalDisplay}>Retour</button>
                    </div>
                </Modal>
            )}
            <div className="page home">
                <div className="rules">
                    <h1 className="title">Règles</h1>
                    <p>Le jeu se joue à 4 joueurs</p>
                    <p>Le jeu se joue avec 3 vélo par coureur</p>
                    <p>Le but du jeu est d'avoir un temps total le plus petit possible</p>
                    <p>Chaque joueur a 5 carte maximum, et repioche quand il n'en a plus</p>
                    <p>Il y a des cases "chance" qui donne un bonus ou malus allant de -3 à +3</p>
                    <p>Les joueur choisise le vélo qu'ils veullent faire avancer à chaque tour</p>
                    <p>Un système d'aspiration permet d'aller plus loin avec la meme carte seconde</p>
                    <p>Le premier joueur à jouer est celui avec la carte seconde la plus élevée</p>
                    <p>Ensuite, l'ordre est défini par le joueur ayant un vélo plus loin que les autres</p>
                    <p>La partie finie quand tous joueurs ont passé la ligne d'arrivée</p>
                </div>
                <div className="start-game">
                    <p>Cliquer sur "nouvelle partie" pour accèder au parametre de la partie</p>
                    <button className={`modal-button ${showSettingsModal ? "active" : ""}`} onClick={handleModalDisplay}>
                        Nouvelle partie
                    </button>
                </div>
            </div>
        </>
    );
};

export default Home;
