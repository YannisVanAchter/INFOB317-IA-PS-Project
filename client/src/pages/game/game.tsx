import React from 'react';

import { TourDeFrance } from '../../Game';
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
console.log(`process.env.REACT_DEBUG: ${process.env.REACT_DEBUG}, isDebug: ${isDebug}x`); // TODO: remove this line once working

type TODO = any;

function Page(props: TODO) {
    // console.log('props', props);
    let players = props.G.players;
    let boardProps = {players: [
        {playerID: 0 as 0, bikes: players[0].bikes.map((bike: any) => bike.position)},
        {playerID: 1 as 1, bikes: players[1].bikes.map((bike: any) => bike.position)},
        {playerID: 2 as 2, bikes: players[2].bikes.map((bike: any) => bike.position)},
        {playerID: 3 as 3, bikes: players[3].bikes.map((bike: any) => bike.position)},
    ]};
    return (
        <div className='board-game'>
            <SideBoard {...props} />
            <TourDeFranceBoard players={boardProps.players}/>
            {/* TODO: Finish the display of cards once Youlan updated Game.ts for simulations */}
            {/* <DisplayHands {...props} /> */}
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