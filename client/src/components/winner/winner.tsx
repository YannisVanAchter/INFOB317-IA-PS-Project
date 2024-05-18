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
        <Modal className={`${props.className} winner`}>
            <h2>Fin de la partie</h2>
            <h3>Classement final</h3>
            <ol>
                {winners.map((player) => (
                    <li key={player} className='player'>
                        <span>{players[player].teamName}</span>
                        {/* <table>
                            <th>
                                <td>{players[player.playerID].teamName}</td>
                                <td>{player.score} points</td>
                            </th>
                            {player.bikeScrore.map((score, index) =>
                                    <tr key={index}>
                                        <td>Vélo: {index}</td>
                                        <td>{score} points</td>
                                    </tr>
                            )}
                        </table> */}
                    </li>
                ))}
            </ol>

            <button onClick={handleRestart}>Rejouer</button>
            <button onClick={handleGoHome}>Retour à l'accueil</button>
        </Modal>
    );
}
