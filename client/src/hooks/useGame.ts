import React, { useState } from 'react';

import { mockUseCardOnBike } from '../Game';
import { Board } from '../Game';

import { Player, GameContextType } from '../types/game';
import { boardKey } from '../types/board';

interface Props {
    player: Player,
    useCard: (bikeIndex: number, cardIndex: number, target: string) => void
}

export function useGame({ player, useCard }: Props): GameContextType {
    let lastBikePosition = Math.max(...player.bikes.map(bike => Board[bike.position].position));
    let bikeIndexInit = player.bikes.findIndex(bike => Board[bike.position].position === lastBikePosition);
    const [currentBikeIndex, setBike] = useState(bikeIndexInit);
    const [currentCardIndex, setCardIndex] = useState(0);

    const applyCardOnBike = (target: boardKey) => {
        console.log("CALLING USE CARD WITH:");
        console.log({ currentBikeIndex, currentCardIndex, target });
        console.log("CALLING NOW");
        try {
            useCard(currentBikeIndex, currentCardIndex, target);
            console.log("AFTER CALLING USE CARD SUCCESS");
            return true;
        } catch (e) {
            console.error(e);
            console.log("AFTER CALLING USE CARD ERROR");
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
