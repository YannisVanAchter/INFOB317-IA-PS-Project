import React, { LegacyRef, useEffect, useRef } from 'react';
import * as SVG from '@svgdotjs/svg.js';
import './board.css';
import { fromMapToSVG } from '../../utils/simonLTransform';
import { boardKey } from '../../utils/simonLTransform';
import { getBoardCase } from '../../Game';
const mapUrl = '../../assets/map.svg';

const playerTeamsEmoticons = [
    "🇧🇪", // Belgique
    "🇳🇱", // Pays-Bas
    "🇩🇪", // Allemagne
    "🇮🇹" // Italie
];

function getSVGElemByID(svg: SVG.Svg, id: string) {
    return svg.findOne(`#${id}`);
}

function TourDeFranceBoard(props: { players: { playerID: 0 | 1 | 2 | 3, bikes: boardKey[] }[] }) {
    const svgRef = useRef(null);

    useEffect(() => {
        if (!svgRef.current) return;

        const svg = SVG.SVG().addTo(svgRef.current).size('100%', '100%');
        const map = svg.image(mapUrl).addTo(svg);

        props.players.forEach((player, index) => {
            player.bikes.forEach((bike, bikeIndex) => {
                const boardCase = getBoardCase(bike);
                const SVGIDBike = fromMapToSVG(bike, boardCase.nbBikesMax as 1 | 2 | 3); // id attribute of the SVG element
                const bikeEmoticon = playerTeamsEmoticons[index];

                const bikeSVG = getSVGElemByID(svg, SVGIDBike) as SVG.Text;
                if (bikeSVG === null) {
                    console.error(`Could not find SVG element with id ${SVGIDBike}`);
                    alert('Could not be able to load the map, please refresh the page')
                    return;
                }
                bikeSVG.text(bikeEmoticon).font({
                    size: 20,
                    anchor: 'middle',
                    leading: 1,
                    fill: 'black',
                    family: 'Arial'
                });
            });
        });
    }, [props.players]);

    // if (props.players === undefined) {
    //     return <Map />;
    // }

    return (
        <div className='board'>
            <div ref={svgRef as unknown as LegacyRef<HTMLDivElement>}></div>
        </div>
    );
}

export default TourDeFranceBoard;
