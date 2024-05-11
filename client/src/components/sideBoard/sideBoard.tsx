import React from 'react';

import { winnerRanking } from '../../Game';

import { players } from '../../data/player';

import { DCtx, Ctx, playerID } from '../../types/game';

import './sideBoard.css';

type TODO = {
    G: DCtx
    ctx: Ctx,
    moves: any,
    className: string
};

function SideBoard(props: TODO) {
    const winners = winnerRanking({G: props.G, ctx: props.ctx});
    const currentPlayer = parseInt(props.ctx.currentPlayer) as playerID;
    return (
        <aside className={`${props.className} winner`}>
            <h2>Current player:</h2>
            <p className='player'>{players[currentPlayer].teamName}</p>
            <h2>Winner ranking</h2>
            <ol>
                {winners.map((player) => (
                    <li key={player} className='player'>
                        <span>{players[player].teamName}</span>
                    </li>
                ))}
            </ol>
        </aside>
    );
}

export default SideBoard;