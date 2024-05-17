import React, { useState } from 'react';

import { Local } from 'boardgame.io/multiplayer';
import { Client } from 'boardgame.io/react';

import { TourDeFrance, bot, mockUseCardOnBike } from '../../Game';
import TourDeFranceBoard from '../../components/board/Board';
import ChatBot from '../../components/bot/bot';
import DisplayHands from '../../components/hands/hands';
import SideBoard from '../../components/sideBoard/sideBoard';
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
};

function Page(props: TODO) {
    let players = props.G.players;
    const currentPlayer = parseInt(props.ctx.currentPlayer) as playerID;

    let boardProps = {
        G: props.G,
        players: [
            { playerID: 0 as 0, bikes: players[0].bikes.map((bike: any) => bike.position) },
            { playerID: 1 as 1, bikes: players[1].bikes.map((bike: any) => bike.position) },
            { playerID: 2 as 2, bikes: players[2].bikes.map((bike: any) => bike.position) },
            { playerID: 3 as 3, bikes: players[3].bikes.map((bike: any) => bike.position) },
        ],
        currentPlayer: currentPlayer,
        availableMoves: [] as string[],
        mockUseCardOnBike: mockUseCardOnBike,
    };

    for (let i = 0; i < props.G.players[currentPlayer].bikes.length; i++) {
        for (let j = 0; j < props.G.players[currentPlayer].hand.length; j++) {
            const availableMoves = mockUseCardOnBike(props.G.players[currentPlayer].bikes[i], props.G.players[currentPlayer].hand[i]);
            boardProps.availableMoves = [...new Set([...boardProps.availableMoves, ...availableMoves])];
        }
    }

    const currentPlayerPlayer = props.G.players[currentPlayer];

    return (
        <div className='board-game'>
            <GameContext.Provider value={useGame({ player: currentPlayerPlayer, useCard: props.moves.useCard })}>
                <SideBoard {...props} />
                <TourDeFranceBoard {...boardProps} />
                <DisplayHands {...props} />
            </GameContext.Provider>
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
        // multiplayer: Local({
        //     bots: {
        //         '0': AIPlayers.includes(0) ? bot : null,
        //         '1': AIPlayers.includes(1) ? bot : null,
        //         '2': AIPlayers.includes(2) ? bot : null,
        //         '3': AIPlayers.includes(3) ? bot : null,
        //     }
        // })
    });

    return <div className='page'>
        <TourDeFranceClient />
        <ChatBot />
    </div>;
}

export default Game;