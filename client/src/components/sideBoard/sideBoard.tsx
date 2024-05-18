import React from 'react';

import { winnerRanking } from '../../Game';

import { players } from '../../data/player';

import { DCtx, Ctx, playerID } from '../../types/game';

import './sideBoard.css';

type TODO = {
    G: DCtx
    ctx: Ctx,
    className?: string
};

function SideBoard(props: TODO) {
    const winners = winnerRanking({G: props.G, ctx: props.ctx});
    const currentPlayer = parseInt(props.ctx.currentPlayer) as playerID;
    return (
        <aside className={`${props.className} winner`}>
            <h2>Pour le tour {props.ctx.turn}, c'est l'équipe:</h2>
            <p className='player'><strong>{players[currentPlayer].teamName}</strong> qui joue</p>
            <h2>Classement temporaire:</h2>
            <ol>
                {winners.map((player, index) => (
                    <li key={index} className='player'>
                        {players[player.playerID].teamName} - {player.score}
                    </li>
                ))}
            </ol>
        </aside>
    );
}

export default SideBoard;