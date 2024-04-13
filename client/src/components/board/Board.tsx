// client/src/Board.tsx
import React from 'react';

import { Context } from '../../Game';

import './board.css';

function TourDeFranceBoard(props: Context) {
    return (
        <>
            <h2>Board</h2>
            {props.G.players.map((player, i) => {
                return (
                    <div key={i}>
                        <h2>Player {player.playerID}</h2>
                        {player.bikes.map((bike, j) => {
                            return (
                                <div key={j}>
                                    <h3>Bike: {j}</h3>
                                    <p>Position: {bike.position}</p>
                                </div>
                            );
                        })}
                    </div>
                );
            })}
        </>
    );
}

export default TourDeFranceBoard;