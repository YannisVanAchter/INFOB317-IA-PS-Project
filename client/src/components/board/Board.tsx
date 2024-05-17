import React from 'react';
import './board.css';
import { ReactSVG } from 'react-svg';

import { BoardProps } from '../../types/board'; 
import { useBoard } from '../../hooks';

function TourDeFranceBoard(props: BoardProps) {
    const { addBikes, addPlayerMoves } = useBoard(props);
    
    return <>
            {/* <Map id="svg-map" className="svg-map" /> */}
            <div className='fill-complete'>
            <ReactSVG 
                src="/map.svg"
                afterInjection={(svg) => {
                    addBikes(svg);
                    addPlayerMoves(svg);
                }}
            />
            </div>
    </>;
}

export default TourDeFranceBoard;
