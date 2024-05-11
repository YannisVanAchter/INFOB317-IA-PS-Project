import React from 'react';

import { winnerRanking } from '../../Game';

import { DCtx, Ctx, } from '../../types/game';

import './sideBoard.css';

type TODO = {
    G: DCtx
    ctx: Ctx,
    moves: any,
    className: string
};

function SideBoard(props: TODO) {
    const winners = winnerRanking({G: props.G, ctx: props.ctx});
    return (
        <aside className={`${props.className} winner`}>
            <h2>Current player:</h2>
            <p className='player'>Player {parseInt(props.ctx.currentPlayer) + 1}</p>
            <h2>Winner ranking</h2>
            <ol>
                {winners.map((player) => (
                    <li key={player} className='player'>
                        <span>Player {player + 1}</span>
                    </li>
                ))}
            </ol>
        </aside>
    );
}

export default SideBoard;