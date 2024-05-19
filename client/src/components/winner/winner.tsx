import React from 'react';

import { Modal } from '../modal/modal';
import { winnerRanking } from '../../Game';
import { players } from '../../data/player';
import { DCtx, Ctx } from '../../types/game';

import './winner.css';

type TODO = {
    G: DCtx
    ctx: Ctx,
    className?: string
};

export function Winner(props: TODO) {
    const winners = winnerRanking({G: props.G, ctx: props.ctx});

    const handleRestart = () => {
        window.location.reload();
    };

    const handleGoHome = () => {
        window.location.href = '/';
    };

    return (
        <Modal className="final-winner">
            <h2>Fin de la partie</h2>
            <h3>Classement final</h3>
            <ol>
                {winners.map((player, index) => (
                    <li key={index} className='player'>
                        <table>
                            <th>
                                <td>{players[player.playerID].teamName}</td>
                                <td>{player.score} points</td>
                            </th>
                            {props.G.players[player.playerID].bikes.map((bike, index) => {
                                    const score = bike.position;
                                    return (
                                    <tr key={index}>
                                        <td>Vélo {index}</td>
                                        <td>{score}</td>
                                    </tr>)
                            })}
                            {
                                <tr>
                                    <td>Points bonus</td>
                                    <td>{props.G.players[player.playerID].bonusPoints}</td>
                                </tr>
                            }
                        </table>
                    </li>
                ))}
            </ol>

            <div>
                <button onClick={handleRestart}>Rejouer</button>
                <button onClick={handleGoHome}>Retour à l'accueil</button>
            </div>
        </Modal>
    );
}
