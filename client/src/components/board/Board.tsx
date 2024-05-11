import React, { useEffect } from 'react';
import './board.css';
import { fromMapToSVG } from '../../utils/simonLTransform';
import { BoardProps, boardKey } from '../../types/board'; 
import Map from '../../assets/map';
import { players } from '../../data/player';

function TourDeFranceBoard(props: BoardProps) {
    const applyCardOnBike = props.applyCardOnBike;
    // Add bikes to the map
    const addBikes = () => {
        let usedMove: boardKey[] = [];
        props.players.forEach(player => {
            player.bikes.forEach(bike => {
                let SvgID = fromMapToSVG(bike, usedMove);
                if (SvgID === "start") 
                    SvgID = `start-${player.playerID}`;
                const element = document.getElementById(SvgID);
                if (element === undefined || element === null) {
                    console.error(`SvgID: ${SvgID} not found`);
                    return;
                }

                const node = document.createElement('img');
                node.src = players[player.playerID].flag;
                node.style.width = '20px';
                node.style.height = '20px';
                node.style.zIndex = '9999';
                node.style.position = 'relative';
                node.style.top = '-20px';
                node.style.left = '-20px';

                // TODO: Check why not working ??
                element.appendChild(node!);

                usedMove.push(bike);
                console.log('element', element, 'has been updated to the player', player.playerID);
            });
        });
    };

    // Add player moves to the map
    const addPlayerMoves = () => {
        let usedMove: boardKey[] = [];
        props.availableMoves.forEach((move, index) => {
            let SvgID: string;
            if (move === "0-B-left")
                SvgID = "start";
            else 
                SvgID= fromMapToSVG(move, usedMove);
            if (usedMove.includes(move) )
                return;

            const element = document.getElementById(SvgID);
            if (element === undefined || element === null) {
                console.error(`SvgID: ${SvgID} not found`);
                return;
            }
            element.classList.add('available-move');
            element.addEventListener('click', () => {
                // console.log('move', props.availableMoves[index]);
                applyCardOnBike(move)
            });
            usedMove.push(move);
            // console.log('element', element, 'has been updated to the available moves', move);
        });
    }

    useEffect(() => {
        addBikes();
        addPlayerMoves();

        // Set img in shadow DOM fit the cases
        const img = document.getElementById('a');
        if (img !== null) {
            img.style.width = 'max-content';
        }
    }, []);
    
    return <>
            <Map className="svg-map" />
    </>;
}

export default TourDeFranceBoard;
