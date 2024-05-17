import React, {useEffect, useState} from "react";
// import  from 'react-svg';

import { fromMapToSVG } from "../utils/simonLTransform";
import { BoardProps, boardKey } from "../types/board";
import { players } from "../data/player";

const useBoard = (props: BoardProps) => {
    const applyCardOnBike = props.applyCardOnBike;
    // Add bikes to the map
    const addBikes = (svg: SVGSVGElement) => {
        let usedMove: boardKey[] = [];
        props.players.forEach(player => {
            player.bikes.forEach((bike, index) => {
                // const svg: SVGSVGElement | null = document.getElementById('svg-map') as SVGSVGElement | null;
                if (svg === null) {
                    console.error('SVG not found');
                    return;
                }
                // Create bike element
                const svgNS = 'http://www.w3.org/2000/svg';
                const group = document.createElementNS(svgNS, 'g');
                const circle = document.createElementNS(svgNS, 'image');

                circle.setAttributeNS(null, 'width', '40');
                circle.setAttributeNS(null, 'height', '40');
                circle.setAttributeNS(null, 'x', '-20');
                circle.setAttributeNS(null, 'y', '-20');
                circle.setAttributeNS(null, 'href', players[player.playerID].flag);
                circle.setAttributeNS(null, 'fill', 'blue');
                // circle.setAttributeNS(null, 'href', "https://upload.wikimedia.org/wikipedia/commons/thumb/6/65/Flag_of_Belgium.svg/1200px-Flag_of_Belgium.svg.png");
                group.setAttributeNS(null, 'id', `${players[player.playerID].teamName}-${index}`);

                svg.appendChild(group); // Add bike to the map (somewhere)
                group.appendChild(circle);

                const newText = document.createElementNS(svgNS, 'text');
                newText.style.fontSize = '20';
                newText.style.fontFamily = 'Arial';
                newText.style.stroke = 'black';

                const textNode = document.createTextNode(`${player.playerID}`);
                newText.appendChild(textNode);
                group.appendChild(newText);

                let SvgID = fromMapToSVG(bike, usedMove);
                if (SvgID === "start") 
                    SvgID = `start-${player.playerID}`;

                // Put bike on the map at the right place
                const element: SVGRectElement = document.querySelector(
                    `#${SvgID} path, #${SvgID} rect, rect#${SvgID}, path#${SvgID}`
                ) as SVGRectElement;

                if (!element) return;

                const { x: svgX, y: svgY } = svg.getBoundingClientRect();
                const { x, y, width, height } = element.getBoundingClientRect();

                //@ts—-ignore
                let point: SVGPoint = svg.createSVGPoint();
                point.x = x;
                point.y = y;

                //@ts—-ignore
                let offset: SVGPoint = svg.createSVGPoint();
                offset.x = svgX + width;
                offset.y = svgY + height;

                //@ts-ignore
                point = point.matrixTransform(svg.getScreenCTM().inverse());
                //@ts-ignore
                offset = offset.matrixTransform(svg.getScreenCTM().inverse());

                //console. log(offset, width, height);
                group.setAttribute(
                    'transform',
                    `translate(${point.x + offset.x / 2}, ${point.y + offset.y / 2})`
                );
                group.style.zIndex = '9999';
            });
        });
    };

    // Add player moves to the map
    const addPlayerMoves = (svg: SVGSVGElement) => {
        let usedMove: boardKey[] = [];
        props.availableMoves.forEach((move, index) => {
            let SvgID: string;
            if (move === "0-B-left")
                SvgID = `start-${props.currentPlayer}`;
            else 
                SvgID= fromMapToSVG(move, usedMove);
            if (usedMove.includes(move))
                return;

            const element = svg.getElementById(SvgID);
            if (element === undefined || element === null) {
                console.error(`SvgID: ${SvgID} not found`);
                return;
            }

            element.setAttributeNS(null, "class", 'available-move');
            element.addEventListener('click', () => {
                console.log('clicked on', move);
                const availableBikes: {bikeIndex: 0|1|2, cardIndex: number[]}[] = [];
                props.G.players[props.currentPlayer].bikes.forEach((bike, bikeIndex) => {
                    const cardIndex = props.G.players[props.currentPlayer].hand.map((card, index) => {
                        if (props.mockUseCardOnBike(bike, card).includes(move)) {
                            return index;
                        }
                    }).filter((index) => index !== undefined) as number[];
                    if (cardIndex.length > 0) {
                        availableBikes.push({bikeIndex: bikeIndex as 0|1|2, cardIndex: cardIndex});
                    }
                })
                if (availableBikes.length === 0) {
                    console.error(`No card found for target ${move}`);
                    return;
                }
                if (availableBikes.length === 1) {
                    applyCardOnBike(move);
                    return;
                }
            });
            usedMove.push(move);
            // console.log('element', element, 'has been updated to the available moves', move);
        });
    }

    return {addBikes, addPlayerMoves};
}

export {useBoard};