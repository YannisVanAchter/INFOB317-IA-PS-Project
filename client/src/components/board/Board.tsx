import React from 'react';
import './board.css';

import Map from '../../assets/map';

import { BoardProps } from '../../types/board'; 
import { useBoard } from '../../hooks';

function TourDeFranceBoard(props: BoardProps) {
    useBoard(props);
    
    return <>
            <Map id="svg-map" className="svg-map" />
    </>;
}

export default TourDeFranceBoard;
