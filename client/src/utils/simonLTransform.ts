/**
 * 
 * Us: 1-A-left
 * 
 * Simon L: c{numéro de case}_{éloignement par rapport au bord gauche de la route en partant de 0}
 */

import { Board } from '../Game';

type position = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 | 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 | 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 | 64 | 65 | 66 | 67 | 68 | 69 | 70 | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 | 82 | 83 | 84 | 85 | 86 | 87 | 88 | 89 | 90 | 91 | 92 | 93 | 94 | 95 | 96;
type caseLetter = 'A' | 'B' | 'C' | 'D';
type side = 'left' | 'center' | 'right';
type boardKey = `${position}-${caseLetter}-${side}` | string;

type svgSide = 0 | 1 | 2 | 3;
type svgId = `c${position}_${svgSide}`;

/**
 * Transform a key from the map to the SVG
 * @param key from de board variable in Game.ts
 * @return the id in the SVG
 */
function fromMapToSVG(key: boardKey, alreadyUsed: boardKey[] = []) {
    if (key === "0-B-left") return "start";

    let [position, caseLetter, _] = key.split('-');
    let svgSide = alreadyUsed.filter((k) => k === key).length as svgSide;

    // read caseLetter
    switch (caseLetter) {
        case 'B':
            svgSide = 1;
            break;
        case 'C':
            svgSide = 2;
            break;
        case 'D':
            svgSide = 3;
            break;
    }

    svgSide = svgSide as svgSide;

    return `c${position}_${svgSide}`;
}

/**
 * Transform a key from the SVG to the map
 * @param id from the SVG
 * @return the key in the board variable in Game.ts
 */
function fromSVGToMap(id: svgId) {
    let [position, side] = id.replace('c', '').split('_');
    let caseLetter: caseLetter = 'A';
    let boardSide: side = 'left';

    // read side
    switch (side) {
        case '1':
            caseLetter = 'B';
            break;
        case '2':
            caseLetter = 'C';
            break;
        case '3':
            caseLetter = 'D';
            break;
    }

    let key = `${position}-${caseLetter}-${boardSide}` as boardKey;

    if (Board[key] === undefined) {
        alert(`Error: ${id} is not a valid id, this give ${key} in the board variable in Game.ts`);  
        // TODO: check caseLetter and boardSide to find the correct key
    }

    return key;
}

export { fromMapToSVG, fromSVGToMap };
export type { boardKey, svgId };

