import React from 'react';
import './board.css';
import { ReactSVG } from 'react-svg';

import { BoardProps } from '../../types/board'; 
import { useBoard } from '../../hooks';

function TourDeFranceBoard(props: BoardProps) {
    const { addBikes, addPlayerMoves } = useBoard(props);
    
    return <>
            <div className='fill-complete'>
            <ReactSVG 
                src="/map.svg"
                afterInjection={(svg) => {
                    console.log('afterInjection', svg);
                    addBikes(svg);
                    addPlayerMoves(svg);
                    console.log('afterInjection added bike and moves', svg);
                }}
            />
            </div>
    </>;
}

export default TourDeFranceBoard;
