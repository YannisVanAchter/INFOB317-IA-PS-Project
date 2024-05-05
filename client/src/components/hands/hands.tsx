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

function DisplayHands(props: TODO) {
    let { G, ctx, copiedG } = props;
    const [hoveredCardIndex, setHoveredCardIndex] = useState<number | null>(null);
    const [clickedCardIndex, setClickedCardIndex] = useState<number | null>(null);

    const copiedGTemp = useCardOnBike({G: copiedG, ctx}, hoveredCardIndex !== null ? hoveredCardIndex : 0);
    const clickedGTemp = useCardOnBike({G: G, ctx}, clickedCardIndex !== null ? clickedCardIndex : 0);

    const handleMouseOver = (card: number, index: number) => {
        console.log('Card:', card, 'Index:', index);
        setHoveredCardIndex(index);
    }
    const handleClickedCard = (card: number, index: number) => {
        console.log('Apply useCardOnBike\nCard:', card, 'Index:', index);
        setClickedCardIndex(index);
    }
    if (copiedGTemp !== null) {
        copiedG = copiedGTemp;
    }
    if (clickedGTemp !== null) {
        G = clickedGTemp;
    }
    
    return (
        <div className='hand'>
            {G.players.map((player, i) => {
                return (
                    <div key={i} className='player'>
                        <h3>Player: {player.playerID + 1}</h3>
                        <p>Hand: </p>
                        <div className='cards'> 
                            {player.hand.map((card, j) => {
                                return (
                                    <div 
                                        key={j} 
                                        className='card'
                                        onClick={() => handleClickedCard(card, j)}
                                        onMouseOver={() => handleMouseOver(card, j)}
                                    >
                                        <CardFront number={card} />
                                    </div>
                                );
                            })}
                        </div>
                    </div>
                );
            })}
        </div>
    );
}

export default DisplayHands;