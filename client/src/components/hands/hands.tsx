import React, { useState } from 'react';

import CardFront from '../../assets/cardFront';
import { DCtx, Ctx } from '../../Game';
import { useCardOnBike } from '../../Game';

import './hands.css';

type TODO = {
    G: DCtx,
    ctx: Ctx,
    copiedG: DCtx,
};

// TODO: finish this component once mock data for simulation is available
function DisplayHands(props: TODO) {
    let { G, ctx, copiedG } = props;
    const currentPlayer = ctx.currentPlayer;
    let players = G.players;
    // const [hoveredCardIndex, setHoveredCardIndex] = useState<number | null>(null);
    // const [clickedCardIndex, setClickedCardIndex] = useState<number | null>(null);

    // const copiedGTemp = useCardOnBike({G: copiedG, ctx}, hoveredCardIndex !== null ? hoveredCardIndex : 0);
    // const clickedGTemp = useCardOnBike({G: G, ctx}, clickedCardIndex !== null ? clickedCardIndex : 0);

    // const handleMouseOver = (card: number, index: number) => {
    //     console.log('Card:', card, 'Index:', index);
    //     setHoveredCardIndex(index);
    // }
    // const handleClickedCard = (card: number, index: number) => {
    //     console.log('Apply useCardOnBike\nCard:', card, 'Index:', index);
    //     setClickedCardIndex(index);
    // }
    // if (copiedGTemp !== null) {
    //     copiedG = copiedGTemp;
    // }
    // if (clickedGTemp !== null) {
    //     G = clickedGTemp;
    // }

    return (
        <div className='hands'>
            {players.map((player, i) => {
                return (
                    <div key={i} className={`player ${currentPlayer === player.toString() ? 'current': ''}`}>
                        <h3>Player: {player.playerID + 1}</h3>
                        <ul className='cards'> 
                            {player.hand.map((card, j) => {
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