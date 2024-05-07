import React from 'react';

import CardFront from '../../assets/cardFront';
import { DCtx, Ctx } from '../../Game';
import { mockUseCardOnBike } from '../../Game'; 
import { deepCopy } from '../../utils/deep_copy';

import './hands.css';

type TODO = {
    G: DCtx,
    ctx: Ctx,
};

// TODO: finish this component once mock data for simulation is available
function DisplayHands(props: TODO) {
    let { G, ctx } = props;
    const currentPlayer = ctx.currentPlayer;
    let players = G.players;

    // TODO: add modal to display choices for player to make when card is played and has multiple destinations

    return (
        <div className='hands'>
            {players.map((player, i) => {
                // if (i.toString() !== currentPlayer) return;
                let hand = deepCopy(player.hand);
                while (hand.length < 5) {
                    console.log(hand);
                    if (hand.length % 2 === 1) { // if even
                        hand = [-1, ...hand, -1];
                    }
                    else {
                        const middle = parseInt(`${hand.length / 2}`);
                        const left = hand.slice(0, middle);
                        const right = hand.slice(middle);
                        hand = [...left, -1, ...right];
                    }
                }
                return (
                    <div key={i} className={`player ${currentPlayer === player.toString() ? 'current': ''}`}>
                        <h3>Player: {player.playerID + 1}</h3>
                        <ul className='cards'> 
                            {hand.map((card, j) => {
                                if (card === -1) return (
                                    <li
                                        key={j}
                                        className={`card card-${j} empty-card`}
                                    >
                                        <CardFront number={-1} />
                                    </li>
                                );
                                return (
                                    <li
                                        key={j}
                                        className={`card card-${j}`}
                                    >
                                        <CardFront number={card} />
                                    </li>
                                );
                            })}
                        </ul>
                    </div>
                );
            })}
        </div>
    );
}

export default DisplayHands;