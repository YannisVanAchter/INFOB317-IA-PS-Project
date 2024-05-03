import { Board } from '../Game';
import type { boardKey } from './simonLTransform';
import { fromMapToSVG, fromSVGToMap } from './simonLTransform';

const SVGMap = require('../assets/map.svg')

describe('fromMapToSVG', () => {
    it('should transform a key from the map to the SVG', () => {
        for (let key of (Object.keys(Board) as boardKey[])) {
            const nbBikesMax = Board[key].nbBikesMax as 1 | 2 | 3;
            const result = fromMapToSVG(key, nbBikesMax);
            const svgElement = SVGMap.getElementById(result);
            expect(svgElement).not.toBeNull();
        }
    });
});

describe('fromSVGToMap', () => {
    it('should transform a key from the SVG to the map', () => {
        for (let elem of SVGMap) {
            const result = fromSVGToMap(elem.id);
            const boardElement = Board[result];
            expect(boardElement).not.toBeNull();
        }
    });
});
