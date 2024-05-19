/**
 * 
 * Us: 1-A-left
 * 
 * Simon L: c{numéro de case}_{éloignement par rapport au bord gauche de la route en partant de 0}
 */
import type { boardKey, svgId, svgSide, caseLetter, side } from '../types/board';
import type { dico, BoardCase } from '../types/game';

/**
 * Transform a key from the map to the SVG
 * @param key from de board variable in Game.ts
 * @return the id in the SVG
 */
function fromMapToSVG(key: boardKey, Board: dico<BoardCase>, alreadyUsed: boardKey[] = []) {
    if (key === "0_B_left") return "start";
    let [position, caseLetter, _] = key.split('_');
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
function fromSVGToMap(id: svgId, Board: dico<BoardCase>) {
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

