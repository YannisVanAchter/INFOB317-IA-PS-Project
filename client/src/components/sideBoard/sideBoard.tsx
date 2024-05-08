import React from 'react';

import { DCtx, Ctx } from '../../Game';
import { winnerRanking } from '../../Game';

import './sideBoard.css';

type TODO = {
    G: DCtx,
    ctx: Ctx,
    moves: any,
    events: any,
    playerID: any,
    isActive: any,
    isMultiplayer: any,
    isConnected: any,
    isConnectedWebRtc: any,
    gameMetadata: any,
    matchData: any,
    isPreview: any,
    className: string,
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