import React, { useEffect } from 'react';

import { Local } from 'boardgame.io/multiplayer';
import { Client } from 'boardgame.io/react';

import { TourDeFrance, bot, mockUseCardOnBike, nbPlayers } from '../../Game';

import TourDeFranceBoard from '../../components/board/Board';
import ChatBot from '../../components/bot/bot';
import DisplayHands from '../../components/hands/hands';
import SideBoard from '../../components/sideBoard/sideBoard';
import { Winner } from '../../components/winner/winner';

import { useGameParams, GameContext } from '../../context';
import { useGame } from '../../hooks/useGame';

import type { DCtx, Ctx, playerID } from '../../types/game';
import type { param } from '../../types/params';

import './game.css';

// TODO: check why this is not working 
// ! process.env.REACT_DEBUG is "undefined" when running "npm start"
// ! Find a way to set the environment variable in the .env file and read it here
const isDebug = process.env.REACT_DEBUG === "true" ?? false; // TODO: once working, replace "|| true" by "|| false" for production
// console.log(`process.env.REACT_DEBUG: ${process.env.REACT_DEBUG}, isDebug: ${isDebug}x`); // TODO: remove this line once working

type TODO = {
    G: DCtx,
    ctx: Ctx,
    moves: any,
    events: any,
    isMultiplayer: boolean,

    // AIPlayers: number[],
    // [key: string]: any,
};

function Page(props: TODO) {
    let players = props.G.players;  
    const currentPlayer = parseInt(props.ctx.currentPlayer) as playerID;

    const currentPlayerPlayer = props.G.players[currentPlayer];

    const { currentBikeIndex, currentCardIndex, setBikeIndex, handleChoiceCard, mockUseCard, applyCardOnBike } = useGame({ 
            player: currentPlayerPlayer, 
            useCard: props.moves.useCard, 
            events: props.events, 
        })

    const { params } = useGameParams();

    if (params.length === 0) {
        alert("Vous devez d'abort définir les paramètres du jeu");
        window.location.href = '/';
        return <></>;
    }

    const AIPlayers = params.filter((param: param) => !param.isHuman).map((param: param) => param.id.toString());

    useEffect(() => {
        if (AIPlayers.includes(currentPlayer.toString())) {
            console.log("AIPlayers.includes(currentPlayer)");
            bot(props.G, props.ctx, currentPlayer.toString()).then((res => {
                setBikeIndex(res.bikeIndex);
                handleChoiceCard(res.cardIndex);
                applyCardOnBike(res.target);
                console.log("bikeIndex: ", res.bikeIndex);
                console.log("cardIndex: ", res.cardIndex);
                console.log("target: ", res.target);
            }))
            // setBikeIndex(bikeIndex);
            // handleChoiceCard(cardIndex);
            // applyCardOnBike(target);
    }});

    let boardProps = {
        G: props.G,
        players: [
            { playerID: 0 as 0, bikes: players[0].bikes.map((bike: any) => bike.position) },
            { playerID: 1 as 1, bikes: players[1].bikes.map((bike: any) => bike.position) },
            { playerID: 2 as 2, bikes: players[2].bikes.map((bike: any) => bike.position) },
            { playerID: 3 as 3, bikes: players[3].bikes.map((bike: any) => bike.position) },
        ],
        currentPlayer: currentPlayer,
        turn: props.ctx.turn,
        availableMoves: [] as { boardKey: string, bikeIndex: number, cardIndex: number }[],
        mockUseCardOnBike: mockUseCardOnBike,
    };

    for (let i = 0; i < props.G.players[currentPlayer].bikes.length; i++) {
        for (let j = 0; j < props.G.players[currentPlayer].hand.length; j++) {
            const bike = props.G.players[currentPlayer].bikes[i];
            const card = props.G.players[currentPlayer].hand[j];
            const moves = mockUseCardOnBike(bike, card);
            moves.forEach((move) => {
                boardProps.availableMoves.push({ boardKey: move, bikeIndex: i, cardIndex: j });
            });
        }
    }

    // If no moves are available, the player has already finish the game
    if (boardProps.availableMoves.length === 0) {
        props.events.pass();
    }

    return (
        <div className='board-game'>
            <GameContext.Provider value={{ currentBikeIndex, currentCardIndex, mockUseCard, setBikeIndex, handleChoiceCard, applyCardOnBike }}>
                <SideBoard {...props} />
                <TourDeFranceBoard {...boardProps} />
                <DisplayHands {...props} />
                {props.ctx.gameover && <Winner G={props.G} ctx={props.ctx} />}
            </GameContext.Provider>
        </div>
    );
}

function Game(props: any) {
    const TourDeFranceClient = Client({
        game: TourDeFrance,
        board: Page,
        numPlayers: nbPlayers,
        debug: isDebug,
    });

    return <div className='page'>
        <TourDeFranceClient />
        <ChatBot />
    </div>;
}

export default Game;