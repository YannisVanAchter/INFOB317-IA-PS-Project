import React, {useEffect, useState} from "react";

import { fromMapToSVG } from "../utils/simonLTransform";
import { BoardProps, boardKey } from "../types/board";
import { players } from "../data/player";

const useBoard = (props: BoardProps) => {
    const applyCardOnBike = props.applyCardOnBike;
    // Add bikes to the map
    const addBikes = () => {
        let usedMove: boardKey[] = [];
        props.players.forEach(player => {
            player.bikes.map((bike, index) => {
                const svg: SVGSVGElement | null = document.getElementById('svg-map') as SVGSVGElement | null;
                if (svg === null) {
                    console.error('SVG not found');
                    return;
                }
                // Create bike element
                const svgNS = 'http://www.w3.org/2000/svg';
                const group = document.createElementNS(svgNS, 'g');
                const circle = document.createElementNS(svgNS, 'img');

                circle.setAttributeNS(null, 'width', '40');
                circle.setAttributeNS(null, 'height', '40');
                circle.setAttributeNS(null, 'x', '-20');
                circle.setAttributeNS(null, 'y', '-20');
                circle.setAttributeNS(null, 'href', players[player.playerID].flag);
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

                // if (SvgID === "start") 
                //     SvgID = `start-${player.playerID}`;
                // const element = document.getElementById(SvgID);
                // if (element === undefined || element === null) {
                //     console.error(`SvgID: ${SvgID} not found`);
                //     return;
                // }

                // const node = document.createElement('img');
                // node.src = players[player.playerID].flag;
                // node.style.width = '20px';
                // node.style.height = '20px';
                // node.style.zIndex = '9999';
                // node.style.position = 'relative';
                // node.style.top = '-20px';
                // node.style.left = '-20px';

                // // TODO: Check why not working ??
                // element.appendChild(node!);

                // usedMove.push(bike);
                // console.log('element', element, 'has been updated to the player', player.playerID);
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

    return null;
}

export {useBoard};