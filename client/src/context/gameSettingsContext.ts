import React, { createContext, useContext } from 'react';
import { useParams } from '../hooks/useParams';

const GameContext = createContext(null);


export const useGameParams = () => useContext(GameContext);
