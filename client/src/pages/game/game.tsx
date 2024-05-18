import React, { useState } from 'react';

import { Local } from 'boardgame.io/multiplayer';
import { Client } from 'boardgame.io/react';

import { TourDeFrance, bot, mockUseCardOnBike, nbPlayers } from '../../Game';
import { players } from '../../data/player';

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
        availableMoves: [] as { boardKey: string, bikeIndex: number, cardIndex: number }[],
        mockUseCardOnBike: mockUseCardOnBike,
    };

    for (let i = 0; i < props.G.players[currentPlayer].bikes.length; i++) {
        for (let j = 0; j < props.G.players[currentPlayer].hand.length; j++) {
            const bike = props.G.players[currentPlayer].bikes[i];
            const card = props.G.players[currentPlayer].hand[j];
            const moves = mockUseCardOnBike(bike, card);
            if (moves.length > 0) {
                moves.forEach((move) => {
                    boardProps.availableMoves.push({ boardKey: move, bikeIndex: i, cardIndex: j });
                });
            }
        }
    }

    // If no moves are available, the player has already finish the game
    if (boardProps.availableMoves.length === 0) {
        props.events.pass();
    }

    const currentPlayerPlayer = props.G.players[currentPlayer];

    return (
        <div className='board-game'>
            <GameContext.Provider value={useGame({ player: currentPlayerPlayer, useCard: props.moves.useCard, events: props.events })}>
                <SideBoard {...props} />
                <TourDeFranceBoard {...boardProps} />
                <DisplayHands {...props} />
                {props.ctx.gameover && <Winner G={props.G} ctx={props.ctx} />}
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
    const playerConfig = Array.from({ length: nbPlayers }, (_, i) => {
        return {
            name: players[i].teamName,
            id: i,
            bot: AIPlayers.includes(i) ? bot : undefined,
        };
    });

    const TourDeFranceClient = Client({
        game: TourDeFrance,
        board: Page,
        numPlayers: nbPlayers,
        debug: isDebug,
        // players: playerConfig,
        // multiplayer: {
        //     local: true,
        //     bots: AIPlayers,
        // }
    });

    // const BoardWithEffects = EffectsBoardWrapper(Page, {
    //     updateStateAfterEffects: true,
    // });

    return <div className='page'>
        <TourDeFranceClient />
        {/* <BoardWithEffects /> */}
        <ChatBot />
    </div>;
}

export default Game;