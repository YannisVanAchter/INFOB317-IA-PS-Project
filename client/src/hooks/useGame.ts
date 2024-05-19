import React, { useState } from 'react';

import { mockUseCardOnBike } from '../Game';
import { Board } from '../Game';

import { useGameParams } from '../context';

import { Player, GameContextType, Context } from '../types/game';
import { boardKey } from '../types/board';

interface Props {
    player: Player,
    useCard: (bikeIndex: number, cardIndex: number, target: string) => void,
    events: any,
}

export function useGame({ player, useCard, events }: Props): GameContextType {
    let lastBikePosition = Math.max(...player.bikes.map(bike => Board[bike.position].position));
    let bikeIndexInit = player.bikes.findIndex(bike => Board[bike.position].position === lastBikePosition);
    let maxCard = Math.max(...player.hand.map(card => card));
    let cardIndexInit = player.hand.findIndex(card => card === maxCard);
    const [currentBikeIndex, setBike] = useState(bikeIndexInit);
    const [currentCardIndex, setCardIndex] = useState(cardIndexInit);
    const { params } = useGameParams();

    const applyCardOnBike = (target: boardKey) => {
        if (!params[player.playerID].isHuman) return true;
        try {
            useCard(currentBikeIndex, currentCardIndex, target);
            return true;
        } catch (e) {
            console.error(e);
            return false;
        }
    }

    const mockUseCard = () => {
        if (currentBikeIndex < 0 || currentBikeIndex >= player.bikes.length) return [];
        if (currentCardIndex < 0 || currentCardIndex >= player.hand.length) return [];
        const bike = player.bikes[currentBikeIndex];
        const card = player.hand[currentCardIndex];
        return mockUseCardOnBike(bike, card);
    };

    const setBikeIndex = (nexIndex: number) => {
        if (nexIndex < 0 || nexIndex >= player.bikes.length) return;
        setBike(nexIndex);
    }

    const handleChoiceCard = (nexIndex: number) => {
        if (nexIndex < 0 || nexIndex >= player.hand.length) return;
        setCardIndex(nexIndex);
    }

    return { currentBikeIndex, currentCardIndex, setBikeIndex, handleChoiceCard, mockUseCard, applyCardOnBike };
}
