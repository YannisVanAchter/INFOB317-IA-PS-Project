import React, { ReactNode, createContext, useContext } from 'react';

import { useParams } from '../hooks/useParams';
import { param } from '../types/params';

interface GameContextType {
    params: param[];
    setParam: (key: number, value: boolean) => void;
}

export const GameSettingsContext = createContext<GameContextType | undefined>(undefined);

export const useGameParams = () => {
    const context = useContext(GameSettingsContext);
    if (!context) {
        alert("Vous devez d'abort définir les paramètres du jeu");
        window.location.href = '/';
        return {params: [], setParam: () => {}};
    }
    return context;
};
