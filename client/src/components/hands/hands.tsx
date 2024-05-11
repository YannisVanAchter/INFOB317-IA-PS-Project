import React, { useEffect, useState } from 'react';

import CardFront from '../../assets/cardFront';
import { DCtx, CardValue, playerID } from '../../types/game';
import { mockUseCardOnBike } from '../../Game'; 
import { deepCopy } from '../../utils/deep_copy';

import { players as PlayerRep } from '../../data/player';

import './hands.css';

type TODO = {
    G: DCtx,
    applyCardOnBike: (target: string) => void
};

function DisplayHands(props: TODO) {
    let { G } = props;
    const currentPlayer = G.currentPlayer.playerID;
    const currentBikeIndex = G.currentPlayer.bikeIndex;
    const currentBike = G.players[currentPlayer].bikes[currentBikeIndex];
    let players = G.players;

    const [displayModal, setDisplayModal] = useState(false);
    const [modalCardValue, setModalCardValue] = useState(0);

    const Modal = () => {
        const availableMoves = mockUseCardOnBike(currentBike, modalCardValue);
        if (availableMoves.length === 1) {
            props.applyCardOnBike(availableMoves[0]);
            setDisplayModal(false);
        }

        const handleChoice = (e: any, move: string) => {
            e.preventDefault();
            props.applyCardOnBike(move);
            setDisplayModal(false);
        }

        return <>
            <div className='modal'>
                <div className='content'>
                    <h2>Carte jouée: {modalCardValue}</h2>
                    <p>L'équipe {PlayerRep[currentPlayer as playerID].teamName} a joué la carte: {modalCardValue}</p>
                    <p>Cette carte mene à plusieurs endroit.</p>
                    <p>Choisissez la destination</p>
                    <ul>
                        {availableMoves.map((move, i) => {
                            return <li 
                                    key={i}
                                    onClick={(event) => handleChoice(event, move)}
                                    >
                                        {move}
                                    </li>
                        })}
                    </ul>
                    <button onClick={() => setDisplayModal(false)}>Close</button>
                </div>
            </div>
        </>
    };

    const handleClickCard = (e: any, playerID: number, cardValue: number) => {
        e.preventDefault();
        if (playerID !== currentPlayer) return;
        setModalCardValue(cardValue);
        setDisplayModal(true);
    };

    //  Effect that apply 1.2 scale to the card when hovered and 0.85 opacity when other cards are hovered
    useEffect(() => {
        const cards_0 = document.querySelectorAll('.card-0');
        const cards_1 = document.querySelectorAll('.card-1');
        const cards_2 = document.querySelectorAll('.card-2');
        const cards_3 = document.querySelectorAll('.card-3');
        const cards_4 = document.querySelectorAll('.card-4');
        const cards = [...cards_0, ...cards_1, ...cards_2, ...cards_3, ...cards_4];
        const classNameOn = 'hovered';
        const classNameOff = 'other-card-hovered';
        cards.forEach((card) => {
            card.setAttribute('style', 'transition: all 0.2s ease-in-out');
            card.addEventListener('mouseover', () => {
                card.className += ` ${classNameOn}`;
                cards.forEach((otherCard) => {
                    if (otherCard.id !== card.id) {
                        otherCard.className += ` ${classNameOff}`;
                    }   
                });
            });
            card.addEventListener('mouseout', () => {
                card.className = card.className.replace(` ${classNameOn}`, '');
                cards.forEach((otherCard) => {
                    if (otherCard.id !== card.id) {
                        otherCard.className = otherCard.className.replace(` ${classNameOff}`, '');
                    }   
                });
            });
        });
    });

    return (
    <>
        {displayModal && <Modal />}
        <div className='hands'>
            {players.map((player, i) => {
                // if (i.toString() !== currentPlayer) return;
                let hand = deepCopy(player.hand);
                while (hand.length < 5) {
                    // console.log(hand);
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
                    <div key={i} className={`player ${currentPlayer === player.playerID ? 'current': ''}`}>
                        <h3>{PlayerRep[i as playerID].teamName}</h3>
                        <ul className='cards'> 
                            {hand.map((card, j) => {
                                    if (card === -1) return (
                                    <li
                                        id={`id-${player.playerID}-${j}`}
                                        key={j}
                                        className={`card empty-card`}
                                    >
                                        <CardFront number={-1} />
                                    </li>
                                );
                                return (
                                    <li
                                        id={`id-${player.playerID}-${j}`}
                                        className={`card`}
                                        onClick={(e) => handleClickCard(e, player.playerID, card)}
                                    >
                                        <CardFront className={`value card-${j}`} number={card as CardValue} />
                                    </li>
                                );
                            })}
                        </ul>
                    </div>
                );
            })}
        </div>
    </>);
}

export default DisplayHands;