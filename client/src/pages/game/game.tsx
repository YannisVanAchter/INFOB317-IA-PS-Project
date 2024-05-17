import React, { useState } from 'react';

import { Local } from 'boardgame.io/multiplayer';
import { Client } from 'boardgame.io/react';

import { TourDeFrance, bot, mockUseCardOnBike } from '../../Game';
import TourDeFranceBoard from '../../components/board/Board';
import ChatBot from '../../components/bot/bot';
import DisplayHands from '../../components/hands/hands';
import SideBoard from '../../components/sideBoard/sideBoard';
import { useGameParams } from '../../context';

import type { DCtx, Ctx, playerID } from '../../types/game';
import type { param } from '../../types/params';

import './game.css';

document.title = 'Tour de France';

// TODO: check why this is not working 
// ! process.env.REACT_DEBUG is "undefined" when running "npm start"
// ! Find a way to set the environment variable in the .env file and read it here
const isDebug = process.env.REACT_DEBUG === "true" || true; // TODO: once working, replace "|| true" by "|| false" for production
// console.log(`process.env.REACT_DEBUG: ${process.env.REACT_DEBUG}, isDebug: ${isDebug}x`); // TODO: remove this line once working

type TODO = {
    G: DCtx,
    ctx: Ctx,
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
    const [selectedCard, setSelectedCard] = useState<{bike: number, cardIndex: number}[]>([]) ;
    let players = props.G.players;
    const currentPlayer = parseInt(props.ctx.currentPlayer) as playerID;

    const applyCardOnBike = (target: string) => {
        // Find card that is being used
        let availableCards: {bike: number, cardIndex: number}[] = [];
        for (let bikeIndex = 0; bikeIndex < props.G.players[currentPlayer].bikes.length; bikeIndex++) {
            for (let i = 0; i < props.G.players[currentPlayer].hand.length; i++) {
                const availableMoves = mockUseCardOnBike(props.G.players[currentPlayer].bikes[bikeIndex], props.G.players[currentPlayer].hand[i]);
                if (availableMoves.includes(target)) {
                    availableCards.push({bike: bikeIndex, cardIndex: i});
                    break;
                }
            }
        }

        if (availableCards.length === 0) {
            console.error(`No card found for target ${target}`);
            return;
        }

        if (availableCards.length === 1) {
            console.log("FROM GAME TSX");
            console.log(availableCards[0]);
            console.log("CALLING USE CARD WITH:");
            console.log(availableCards[0]);
            console.log(target);
            console.log("CALLING NOW");
            let {bike, cardIndex} = availableCards[0];
            props.moves.useCard(bike, cardIndex, target);
        }
        else {
            let cards = [availableCards[0]];
            for (let i = 1; i < availableCards.length; i++) {
                if (props.G.players[currentPlayer].hand[availableCards[i].cardIndex] !== props.G.players[currentPlayer].hand[availableCards[i - 1].cardIndex]) {
                    cards.push(availableCards[i]);
                }
            }

            if (cards.length === 1) {
                console.log("FROM GAMES TSX")
                console.log(cards[0]);
                let {bike, cardIndex} = cards[0];
                props.moves.useCard(bike, cardIndex, target);
                return;
            }

            setSelectedCard(availableCards);
            setMultipleCardsAllowed(true);
        };
    }
    
    let boardProps = {
        players: [
            {playerID: 0 as 0, bikes: players[0].bikes.map((bike: any) => bike.position)},
            {playerID: 1 as 1, bikes: players[1].bikes.map((bike: any) => bike.position)},
            {playerID: 2 as 2, bikes: players[2].bikes.map((bike: any) => bike.position)},
            {playerID: 3 as 3, bikes: players[3].bikes.map((bike: any) => bike.position)},
        ],
        currentPlayer: currentPlayer,
        availableMoves: [] as string[],
        applyCardOnBike: applyCardOnBike
    };

    for (let i = 0; i < props.G.players[currentPlayer].bikes.length; i++) {
        for (let j = 0; j < props.G.players[currentPlayer].hand.length; j++) {
            const availableMoves = mockUseCardOnBike(props.G.players[currentPlayer].bikes[i], props.G.players[currentPlayer].hand[i]);
            // console.log(props.G.players[currentPlayer]);
            // console.log(props.G.players[currentPlayer].hand[i]);
            // console.log(availableMoves);
            boardProps.availableMoves = [...new Set([...boardProps.availableMoves, ...availableMoves])];
        }
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
    const { params } = useGameParams();
    if (params.length === 0) {
        alert("Vous devez d'abort définir les paramètres du jeu");
        window.location.href = '/';
        return <></>;
    }

    const AIPlayers = params.filter((param: param) => !param.isHuman).map((param: param) => param.id);

    const TourDeFranceClient = Client({
        game: TourDeFrance,
        board: Page,
        numPlayers: 4,
        debug: isDebug,
        multiplayer: Local({
            bots: {
                '0': AIPlayers.includes(0) ? bot : null,
                '1': AIPlayers.includes(1) ? bot : null,
                '2': AIPlayers.includes(2) ? bot : null,
                '3': AIPlayers.includes(3) ? bot : null,
            }
        })
    });

    return <div className='page'>
        <TourDeFranceClient />
        <ChatBot />
    </div>;
}

export default Game;