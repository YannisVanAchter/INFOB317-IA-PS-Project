// client/src/Board.tsx
import React from 'react';

import './board.css';
import { fromMapToSVG } from '../../utils/simonLTransform';
import { boardKey } from '../../utils/simonLTransform';
import { getBoardCase } from '../../Game';

import Map from '../../assets/map';
// import Map from '../../assets/map.svg';

const player_teams_emoticons = {
    0: "&#127463 &#127466",// belgique
    1: "&#127475 &#127473",// Holland
    2: "&#127465 &#127466",// Allemagne
    3: "&#127470 &#127481"// Italie
};

function TourDeFranceBoard(props: {players: {playerID: 0 | 1 | 2 | 3, bikes: boardKey[]}[]}) {
    if (props.players === undefined) {
        return (
            <>
                <Map />
            </>
        );
    }
    // Add bikes to the map with the player's color
    for (let player of props.players) {
        for (let bike of player.bikes) {
            let boardCase = getBoardCase(bike);
            let svgId = fromMapToSVG(bike, boardCase.nbBikesMax as 1 | 2 | 3);

            // Add the corresponding bike to the map
            // let svgElement = document.getElementById(svgId);
            // if (svgElement === null) {
            //     console.error(`svgElement with id ${svgId} not found`);
            //     continue;
            // }
            // let playerEmoticons = player_teams_emoticons[player.playerID];
            // let bikeElementTemplate = `
            // <text x="50%" y="50%" text-anchor="middle" dominant-baseline="middle" font-size="1.5em">
            //     ${playerEmoticons}
            // </text>
            // ` as unknown as SVGElement;
            // svgElement.appendChild(bikeElementTemplate);
        }
    }

    return (
        <>
            <Map />
        </>
    );
}

export default TourDeFranceBoard;