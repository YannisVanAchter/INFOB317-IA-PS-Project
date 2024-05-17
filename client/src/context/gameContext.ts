import React, { useContext, createContext } from 'react';

import { GameContextType } from '../types/game';
import { boardKey } from '../types/board';

export const GameContext = createContext<GameContextType>(null as any);

export const useGameContext = (mock=false) => {
    const context = useContext(GameContext);
    if (!context) {
        alert("Vous devez d'abort définir les paramètres du jeu");
        if (!mock)
            window.location.href = '/';
        return {
            currentBikeIndex: 0,
            currentCardIndex: 0,
            setBikeIndex: (nexIndex: number) => {},
            handleChoiceCard: (nexIndex: number) => {},
            mockUseCard: () => [],
            applyCardOnBike: (target: boardKey) => {
                console.log("APPLY CARD ON BIKE, BUT NO CONTEXT");
                return false},
        };
    }
    return context;
};
