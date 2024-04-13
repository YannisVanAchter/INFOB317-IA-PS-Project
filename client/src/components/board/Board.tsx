// client/src/Board.tsx
import React from 'react';

import { Context } from '../../Game';

import './board.css';

function TourDeFranceBoard(props: Context) {
    return (
        <>
            <h2>Board</h2>
            <div className='board'>
                {props.G.players.map((player, i) => {
                    return (
                        <div key={i}>
                            <h2>Player {player.playerID + 1}</h2>
                            {player.bikes.map((bike, j) => {
                                return (
                                    <div key={j}>
                                        <h3>Bike: {j + 1}</h3>
                                        <p>Position: {bike.position}</p>
                                        <p>Turn: {bike.turn}</p>
                                    </div>
                                );
                            })}
                        </div>
                    );
                })}
            </div>
        </>
    );
}

export default TourDeFranceBoard;