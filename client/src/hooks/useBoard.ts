import React, { useContext } from "react";
// import  from 'react-svg';

import { GameContext } from "../context/gameContext";
import { useGameParams } from "../context";
import { fromMapToSVG } from "../utils/simonLTransform";
import { BoardProps, boardKey } from "../types/board";
import { players } from "../data/player";

const useBoard = (props: BoardProps) => {
    const { setBikeIndex, handleChoiceCard, applyCardOnBike } = useContext(GameContext);
    const { params } = useGameParams();
    // Add bikes to the map
    const addBikes = (svg: SVGSVGElement) => {
        if (svg === null) {
            console.error('SVG not found');
            return;
        }
        const svgNS = 'http://www.w3.org/2000/svg';
        let usedMove: boardKey[] = [];
        props.players.forEach(player => {
            player.bikes.forEach((bike, index) => {
                // Credits to Simon Loir for the following code to add and place bikes on the map
                // Create bike element
                const group = document.createElementNS(svgNS, 'g');
                const circle = document.createElementNS(svgNS, 'image');

                circle.setAttributeNS(null, 'width', '40');
                circle.setAttributeNS(null, 'height', '40');
                circle.setAttributeNS(null, 'x', '-20');
                circle.setAttributeNS(null, 'y', '-20');
                circle.setAttributeNS(null, 'href', players[player.playerID].flag);
                circle.setAttributeNS(null, 'fill', 'blue');
                group.setAttributeNS(null, 'id', `${players[player.playerID].teamName}-${index}`);

                group.appendChild(circle);
                svg.appendChild(group); // Add bike to the map (somewhere)

                // Add bike index in the bike
                const newText = document.createElementNS(svgNS, 'text');
                newText.style.fontSize = '20';
                newText.style.fontFamily = 'Arial';
                newText.style.stroke = 'black';
                newText.style.transform = 'translate(-5px, 5.5px)';

                const textNode = document.createTextNode(`${index + 1}`);
                newText.appendChild(textNode);
                group.appendChild(newText);

                let SvgID = fromMapToSVG(bike, props.board, usedMove);
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
        if (!params[props.currentPlayer].isHuman) return;
        
        let usedMove: boardKey[] = [];
        props.availableMoves.forEach((move, index) => {
            let SvgID: string;
            if (move.boardKey === "0-B-left")
                SvgID = `start-${props.currentPlayer}`;
            else 
                SvgID= fromMapToSVG(move.boardKey, props.board, usedMove);
            if (usedMove.includes(move.boardKey))
                return;

            const element = svg.getElementById(SvgID);
            if (element === undefined || element === null) {
                console.error(`SvgID: ${SvgID} not found`);
                return;
            }

            element.setAttributeNS(null, "class", 'available-move');
            element.addEventListener('click', () => {
                setBikeIndex(move.bikeIndex);
                handleChoiceCard(move.cardIndex);
                applyCardOnBike(move.boardKey);
            });
            usedMove.push(move.boardKey);
            // console.log('element', element, 'has been updated to the available moves', move);
        });
    }

    return {addBikes, addPlayerMoves};
}

export {useBoard};