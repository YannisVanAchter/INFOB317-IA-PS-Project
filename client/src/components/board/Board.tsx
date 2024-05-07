import React, { LegacyRef, useEffect, useState, useRef } from 'react';
import { SVG, Svg, Dom, extend as SVGextend, Element as SVGElement } from '@svgdotjs/svg.js';
import './board.css';
import { fromMapToSVG } from '../../utils/simonLTransform';
import { boardKey } from '../../utils/simonLTransform';
import { getBoardCase } from '../../Game';
import Map from '../../assets/map.svg';
import mapPng from '../../assets/map.png';

const playerTeamsEmoticons = [
    "🇧🇪", // Belgique
    "🇳🇱", // Pays-Bas
    "🇩🇪", // Allemagne
    "🇮🇹" // Italie
];

type BoardProps = {
    players: { playerID: 0 | 1 | 2 | 3, bikes: boardKey[] }[],
    currentPlayer: 0 | 1 | 2 | 3
    availableMoves: boardKey[]
};

function TourDeFranceBoard(props: BoardProps) {
    // load svg map 
    console.log('TourDeFranceBoard');
    let [map, setMap] = useState<Svg|null>(null);
    let [bikes, setBikes] = useState<Element[]>([]);
    let [playerMoves, setPlayerMoves] = useState<Dom[]>([]);
    let [currentPlayer, setCurrentPlayer] = useState<0 | 1 | 2 | 3>(parseInt(props.currentPlayer.toString()) as 0 | 1 | 2 | 3);

    // Add bikes to the map
    useEffect(() => {
        if (map) {
            bikes = props.players.map(player => {
                return player.bikes.map(bike => {
                    const caseElement = getBoardCase(bike);
                    const SvgID = fromMapToSVG(bike, caseElement.nbBikesMax as 1|2|3);
                    if (map === null) 
                        map = Map as unknown as Svg;
                    console.log(`SvgID: ${SvgID}`);
                    const bikeFuturePosition = map.data(`#${SvgID}`);
                    if (bikeFuturePosition === undefined) {
                        console.error(`SvgID: ${SvgID} not found`);
                        return null;
                    }
                    const playerEmoticon = playerTeamsEmoticons[player.playerID];
                    const bikeSVG = map.group();
                    bikeSVG.text(playerEmoticon).addClass('player-emoticon');
                    bikeSVG.move(bikeFuturePosition.cx(), bikeFuturePosition.cy());

                    bikeFuturePosition.add(bikeSVG);
                    return bikeFuturePosition;
                }) as unknown as Element[];
            }).flat();
            setBikes(bikes);
        }
    }, [map, props.players]);

    // Add player moves to the map
    useEffect(() => {
        if (map) {
            playerMoves = props.availableMoves.map(move => {
                if (map === null) 
                    map = Map as unknown as Svg;
                const caseElement = getBoardCase(move);
                const SvgID = fromMapToSVG(move, caseElement.nbBikesMax as 1|2|3);
                const moveElement = map.data(`#${SvgID}`) as Dom;
                moveElement.addClass('move');
                return moveElement;
            }).filter(moveElement => moveElement !== null) as Dom[];
            setPlayerMoves(playerMoves);
        }
    }, [map, props.availableMoves]);

    // Add the map to the DOM
    useEffect(() => {
        let isMounted = true;
        fetch(Map)
            .then(response => response.text())
            .then(data => {
                if (isMounted) {
                    if (map) {
                        map.remove();
                    }
                    map = SVG()
                        .addTo('#board-map')
                        .size('100%', '100%')
                        .front()
                        .svg(data);
                    setMap(map);
                }
            });

        return () => {
            isMounted = false;
        };
    }, []);

    // Update the current player
    useEffect(() => {
        setCurrentPlayer(props.currentPlayer);
    }, [props.currentPlayer]);
    
    return <>
        <img id="board-map" className='board' src={mapPng} alt='Tour de France plateau' />
    </>;
}

export default TourDeFranceBoard;
