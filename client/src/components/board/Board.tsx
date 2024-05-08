import React, { LegacyRef, useEffect, useState, useRef } from 'react';
import { SVG, Svg, Dom, extend as SVGextend, Element as SVGElement } from '@svgdotjs/svg.js';
import './board.css';
import { fromMapToSVG } from '../../utils/simonLTransform';
import { boardKey } from '../../utils/simonLTransform';
import { getBoardCase } from '../../Game';
import Map from '../../assets/map.svg';
import mapPng from '../../assets/map.png';

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
};

function TourDeFranceBoard(props: BoardProps) {
    // load svg map 
    console.log('props', props);
    let [map, setMap] = useState<Svg|null>(null);
    let [bikes, setBikes] = useState<Element[]>([]);
    let [playerMoves, setPlayerMoves] = useState<any[]>([]);
    let [currentPlayer, setCurrentPlayer] = useState<0 | 1 | 2 | 3>(parseInt(props.currentPlayer.toString()) as 0 | 1 | 2 | 3);

    let svgMapRendered = useRef(false);

    // Add bikes to the map
    // useEffect(() => {
    //     if (map) {
    //         bikes = props.players.map(player => {
    //             return player.bikes.map(bike => {
    //                 if (bike === "0-B-left") return; // Skip the first position (start line)
    //                 const caseElement = getBoardCase(bike);
    //                 const SvgID = fromMapToSVG(bike, caseElement.nbBikesMax as 1|2|3);
    //                 if (map === null) 
    //                     map = Map as unknown as Svg;
    //                 console.log(`SvgID: ${SvgID}`);
    //                 const bikeFuturePosition = document.getElementById(SvgID);
    //                 if (bikeFuturePosition === undefined) {
    //                     console.error(`SvgID: ${SvgID} not found`);
    //                     return null;
    //                 }
    //                 const playerEmoticon = playerTeamsEmoticons[player.playerID];
    //                 const playerEmoticonElement = document.createElement('span');
    //                 playerEmoticonElement.textContent = playerEmoticon;
    //                 bikeFuturePosition!.appendChild(playerEmoticonElement);

    //                 return bikeFuturePosition;
    //             }) as unknown as Element[];
    //         }).flat();
    //         setBikes(bikes);
    //     }
    // }, [map, props.players]);

    // Add player moves to the map
    useEffect(() => {
        if (map) {
            playerMoves = props.availableMoves.map(move => {
                if (map === null)
                    return ;
                const caseElement = getBoardCase(move);
                const SvgID = fromMapToSVG(move, caseElement.nbBikesMax as 1|2|3) as string;
                const moveElement = document.getElementById(SvgID);
                if (!moveElement) {
                    console.error(`Element: ${moveElement} for ${SvgID} not found`);
                    return null;
                }
                console.log(`Element: ${moveElement} for ${SvgID}`);
                moveElement!.classList.add('move');
                return moveElement;
            }).filter(moveElement => moveElement !== null);
            setPlayerMoves(playerMoves);
        }
    }, [map, props.availableMoves]);

    // Add the map to the DOM
    useEffect(() => {
        if (!svgMapRendered.current) {
            const mapElement = document.getElementById('board-map');
            if (mapElement === null) {
                console.error('Map element not found');
                return;
            }

            // Charger l'image SVG
            fetch('/static/media/map.94d8e552a56358b2467a849395bf7b1b.svg')
                .then(response => response.text())
                .then(data => {
                    let map = SVG().svg(data);
                    map.addClass('svg-map');
                    map.attr('id', 'map');
                    if (document.getElementById('map') === null) {
                        const startLine = `
                            <g id="start-line" transform="translate(750, 750)">
                            </g>
                        `;
                        map.node.innerHTML += startLine;
                        mapElement.appendChild(map.node);
                    }
                    setMap(map);
                    svgMapRendered.current = true;
                })
                .catch(error => console.error('Error loading SVG:', error));
        }
    }, []);

    // Update the current player
    useEffect(() => {
        setCurrentPlayer(props.currentPlayer);
    }, [props.currentPlayer]);
    
    return <>
        <div id="board-map" className='board'></div>
    </>;
}

export default TourDeFranceBoard;
