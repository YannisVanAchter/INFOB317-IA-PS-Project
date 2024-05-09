import React, { useEffect } from 'react';
import './board.css';
import { fromMapToSVG } from '../../utils/simonLTransform';
import { boardKey } from '../../utils/simonLTransform'; // Type definition
import Map from '../../assets/map';
import { Svg } from '@svgdotjs/svg.js';

const playerTeamsEmoticons = [
    "https://hatscripts.github.io/circle-flags/flags/be.svg", // Belgique
    "https://hatscripts.github.io/circle-flags/flags/nl.svg", // Pays-Bas
    "https://hatscripts.github.io/circle-flags/flags/de.svg", // Allemagne
    "https://hatscripts.github.io/circle-flags/flags/it.svg" // Italie
];

type BoardProps = {
    players: { playerID: 0 | 1 | 2 | 3, bikes: boardKey[] }[],
    currentPlayer: 0 | 1 | 2 | 3,
    availableMoves: boardKey[],
    applyCardOnBike: (target: boardKey) => void
};

function TourDeFranceBoard(props: BoardProps) {
    const applyCardOnBike = props.applyCardOnBike;
    // Add bikes to the map
    const addBikes = () => {
        let usedMove: string[] = [];
        props.players.forEach(player => {
            player.bikes.forEach(bike => {
                let SvgID = fromMapToSVG(bike, usedMove);
                if (SvgID === "start") 
                    SvgID = `start-${props.currentPlayer}`;
                const element = document.getElementById(SvgID);
                if (element === undefined || element === null) {
                    console.error(`SvgID: ${SvgID} not found`);
                    return;
                }
                const img = `
                <image 
                    x="-20" y="-20" 
                    height="40" width="40" 
                    href="${playerTeamsEmoticons[player.playerID]}"
                    style="z-index: 9999;"
                ></image>
                `;
                const template = document.createElement('template');
                template.innerHTML = img;
                const node = template.content.firstChild;

                // TODO: Check why not working ??
                element.appendChild(node!);

                usedMove.push(bike);
                // console.log('element', element, 'has been updated to the player', player.playerID);
            });
        });
    };

    // Add player moves to the map
    const addPlayerMoves = () => {
        let usedMove: string[] = [];
        props.availableMoves.forEach((move, index) => {
            let SvgID: string;
            if (move === "0-B-left")
                SvgID = "start";
            else 
                SvgID= fromMapToSVG(move, usedMove);

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
    }, []);
    
    return <>
            <Map className="svg-map" />
    </>;
}

export default TourDeFranceBoard;
