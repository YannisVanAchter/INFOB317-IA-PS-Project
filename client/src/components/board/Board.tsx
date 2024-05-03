// client/src/Board.tsx
import React, { useState } from 'react';

import { Context, DCtx, Ctx, winnerRanking, useCardOnBike } from '../../Game';

import './board.css';
import { deepCopy } from '../../utils/deep_copy';

// const mapPath = "../../assets/map.svg";
// const Map = require(mapPath);
import Map from '../../assets/map';

type TODO = {
    G: DCtx,
    ctx: Ctx,
    copiedG: DCtx,
};

function displayCard(card: number) {
    return <>{card}</>
}

function DisplayHand(props: TODO) {
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
                                        {displayCard(card)}
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

function ListBikes(props: DCtx) {
    return (
        <aside>
            {props.players.map((player, i) => {
                return (
                    <div key={i} className='player'>
                        <h3>Player: {player.playerID + 1}</h3>
                        {player.bikes.map((bike, j) => {
                            return (
                                <div key={j}>
                                    <p>Bike: {bike.position}c</p>
                                </div>
                            );
                        })}
                    </div>
                );
            })}
        </aside>
    );
}

function TourDeFranceBoard({ G, ctx, moves, events, playerID, isActive, isMultiplayer }: any) {
    let copiedG: DCtx = deepCopy(G);

    return (
        <>
            <h2>Board</h2>
            <ListBikes {...copiedG} />

            <Map />
            {/* <img src={mapPath} alt="Map" /> */}

            {/* <DisplayHand copiedG={copiedG} G={G} ctx={ctx}  /> */}
        </>
    );
}

export default TourDeFranceBoard;