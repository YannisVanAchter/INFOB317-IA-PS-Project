import React, { useState } from 'react';

import { TourDeFrance, mockUseCardOnBike } from '../../Game';
import { Client } from 'boardgame.io/react';
import TourDeFranceBoard from '../../components/board/Board';
import ChatBot from '../../components/bot/bot';
import DisplayHands from '../../components/hands/hands';
import SideBoard from '../../components/sideBoard/sideBoard';

import './game.css';

document.title = 'Tour de France';

// TODO: check why this is not working 
// ! process.env.REACT_DEBUG is "undefined" when running "npm start"
// ! Find a way to set the environment variable in the .env file and read it here
const isDebug = process.env.REACT_DEBUG === "true" || true; // TODO: once working, replace "|| true" by "|| false" for production
// console.log(`process.env.REACT_DEBUG: ${process.env.REACT_DEBUG}, isDebug: ${isDebug}x`); // TODO: remove this line once working

type TODO = {
    G: any,
    ctx: any,
    moves: any,
};

const Modal = ({selectedCard, applyCardOnBike, setMultipleCardsAllowed, G, ctx}: any) => {
    return (
        <div className='modal'>
            <div className='content'>
                <h2>Plusieurs cartes mènent à cet destination</h2>
                <p>Choisissez la carte à utiliser</p>
                <ul>
                    {selectedCard.map((cardIndex: number) => (
                        <li key={cardIndex}>
                            <button onClick={() => {
                                applyCardOnBike({G, ctx}, cardIndex);
                                setMultipleCardsAllowed(false);
                            }}>
                                {G.players[G.currentPlayer.playerID].hand[cardIndex]}
                            </button>
                        </li>
                    ))}
                </ul>
            </div>
        </div>
    );
}

function Page(props: TODO) {
    const [multipleCardsAllowed, setMultipleCardsAllowed] = useState(false) ;
    const [selectedCard, setSelectedCard] = useState<number[]>([]) ;
    let players = props.G.players;
    const currentPlayer = props.G.currentPlayer;

    const applyCardOnBike = (target: string) => {
        // Find card that is being used
        let availableCards: number[] = [];
        for (let i = 0; i < props.G.players[currentPlayer.playerID].hand.length; i++) {
            const availableMoves = mockUseCardOnBike(props.G.players[currentPlayer.playerID].bikes[currentPlayer.bikeIndex], props.G.players[currentPlayer.playerID].hand[i]);
            if (availableMoves.includes(target)) {
                availableCards.push(i);
                break;
            }
        }

        if (availableCards.length === 0) {
            console.error(`No card found for target ${target}`);
            return;
        }

        if (availableCards.length === 1) {
            props.moves.useCard({ ...props }, availableCards[0]);
        }
        else {
            setSelectedCard(availableCards);
            setMultipleCardsAllowed(true);
        };
    }
    
    let boardProps = {players: [
        {playerID: 0 as 0, bikes: players[0].bikes.map((bike: any) => bike.position)},
        {playerID: 1 as 1, bikes: players[1].bikes.map((bike: any) => bike.position)},
        {playerID: 2 as 2, bikes: players[2].bikes.map((bike: any) => bike.position)},
        {playerID: 3 as 3, bikes: players[3].bikes.map((bike: any) => bike.position)},
    ],
    currentPlayer: currentPlayer,
    availableMoves: [] as string[],
    applyCardOnBike: applyCardOnBike
    };

    for (let i = 0; i < props.G.players[currentPlayer.playerID].hand.length; i++) {
        const availableMoves = mockUseCardOnBike(props.G.players[currentPlayer.playerID].bikes[currentPlayer.bikeIndex], props.G.players[currentPlayer.playerID].hand[i]);
        boardProps.availableMoves = [...boardProps.availableMoves, ...availableMoves];
    }

    return (
        <div className='board-game'>
            <SideBoard {...props} className='' />
            <TourDeFranceBoard {...boardProps}/>
            <DisplayHands {...props} applyCardOnBike={applyCardOnBike}/>
            {multipleCardsAllowed && <Modal selectedCard={selectedCard} applyCardOnBike={applyCardOnBike} setMultipleCardsAllowed={setMultipleCardsAllowed} G={props.G} ctx={props.ctx} />}
        </div>
    );
}

function Game(props: any) {
    // TODO: build the game depending on the parameters (who is AI, who is human, etc.)
    const TourDeFranceClient = Client({
        game: TourDeFrance,
        board: Page,
        numPlayers: 4,
        // debug: isDebug,
        debug: false,
    });

    return <div>
        <TourDeFranceClient />
        <ChatBot />
    </div>;
}

export default Game;