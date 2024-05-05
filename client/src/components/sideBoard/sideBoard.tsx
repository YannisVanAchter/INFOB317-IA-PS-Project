import React from 'react';

import { DCtx } from '../../Game';

import './sideBoard.css';

type TODO = {
    G: DCtx,
    ctx: any,
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
};

function SideBoard(props: TODO) {
    return (
        <aside>
            {props.G.players.map((player, i) => {
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

export default SideBoard;