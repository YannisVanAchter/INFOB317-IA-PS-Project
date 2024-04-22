import { FnContext } from 'boardgame.io';
import { shuffle } from './utils/shuffle';

// Constants
const nbCases = 95;
const nbReduceMax = 9;

const nbBikes = 3;

const nbPlayers = 4;

const CardsSeconds = [
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9,
    10,
    11,
    12,
];
const nbCards = 96;
const nbCardsFirsthand = 5;
const nbCardsHand = 3;
const CardsDeck = [...Array(nbCards / CardsSeconds.length)].flatMap(() => CardsSeconds); // 8x 1-12 = 96 cards

// const board = defineBoard(nbCases);

// Types
type BoardIndex = [
    position: number,
    letter: "A"|"B"|"C"|"D", // Default A
    side: "left"|"right" // Default left (Separation of the board in two sides (sprint zone))
]

interface BoardCase {
    position: number;
    letter?: "A"|"B"|"C"|"D"; // Letter of the case (A, B, C or D)
    next?: Array<string>; // Index of the next cases
    luck: boolean; // Draw luck card
    side?: "intern"|"extern"; // Side of the case (dans le sens de la course)
    nbBikesMax: number; // Number of bikes that can be on the case
    nbBikes: number; // Number of bikes on the case (default 0)
}

// type BoardDictionnary= Record<BoardIndex, BoardCase>;
// type dico={[key: BoardIndex ]: BoardCase};
type dico<BoardCase>={
    [key: string]:BoardCase;
}

let Board: dico<BoardCase>={
    '[1,"A","left"]':{position: 1, luck: false, nbBikesMax: 3, nbBikes:0, next: ['[2,"A","left"]']},
    '[2,"A","left"]':{position: 2, luck: false, nbBikesMax:3, nbBikes:0, next: ['[3,"A","left"]']},
    '[3,"A","left"]':{position: 3, luck: false, nbBikesMax:3, nbBikes:0, next: ['[4,"A","left"]']},
    '[4,"A","left"]':{position: 4, luck: false, nbBikesMax:3, nbBikes:0, next: ['[5,"A","left"]']},
    '[5,"A","left"]':{position: 5, luck: false, nbBikesMax:3, nbBikes:0, next: ['[6,"A","left"]']},
    '[6,"A","left"]':{position: 6, luck: false, nbBikesMax:3, nbBikes:0, next: ['[7,"A","left"]']},
    '[7,"A","left"]':{position: 7, luck: false, nbBikesMax:3, nbBikes:0, next: ['[8,"A","left"]']},
    '[8,"A","left"]':{position: 8, luck: false, nbBikesMax:3, nbBikes:0, next: ['[9,"A","left"]','[9,"C","left"]']},
    '[9,"A","left"]':{position: 9, luck: true, nbBikesMax:1, nbBikes:0, next: ['[10,"A","left"]','[10,"C","left"]']},
    '[9,"B","left"]':{position: 9, luck: false, nbBikesMax:1, nbBikes:0, next: ['[10,"A","left"]','[10,"C","left"]']},
    '[9,"C","left"]':{position: 9, luck: false, nbBikesMax:1, nbBikes:0, next: ['[9,"B","left"]']},
    '[10,"A","left"]':{position: 10, luck: true, nbBikesMax:1, nbBikes:0, next: ['[11,"A","left"]']},
    '[10,"B","left"]':{position: 10, luck: false, nbBikesMax:1, nbBikes:0, next: ['[11,"A","left"]']},
    '[10,"C","left"]':{position: 10, luck: false, nbBikesMax:1, nbBikes:0, next: ['[10,"B","left"]']},
    '[11,"A","left"]':{position: 11, luck: true, nbBikesMax:2, nbBikes:0, next: ['[12,"A","left"]']},
    '[12,"A","left"]':{position: 12, luck: true, nbBikesMax:2, nbBikes:0, next: ['[13,"A","left"]']},
    '[13,"A","left"]':{position: 13, luck: false, nbBikesMax:2, nbBikes:0, next: ['[14,"A","left"]']},
    '[14,"A","left"]':{position: 14, luck: false, nbBikesMax:2, nbBikes:0, next: ['[15,"A","left"]']},
    // chance vrai pour la case de droite seulement (15 et 16)
    '[15,"A","left"]':{position: 15, luck: true, nbBikesMax:2, nbBikes:0, next: ['[16,"A","left"]']},
    '[16,"A","left"]':{position: 16, luck: true, nbBikesMax:2, nbBikes:0, next: ['[17,"A","left"]']},
    '[17,"A","left"]':{position: 17, luck: false, nbBikesMax:2, nbBikes:0, next: ['[18,"A","left"]']},
    '[18,"A","left"]':{position: 18, luck: false, nbBikesMax:2, nbBikes:0, next: ['[19,"A","left"]']},
    //  chance sur ceux de droite (19)
    '[19,"A","left"]':{position: 19, luck: true, nbBikesMax: 3, nbBikes:0, next: ['[20,"A","left"]']},
    '[20,"A","left"]':{position: 20, luck: false, nbBikesMax: 3, nbBikes:0, next: ['[21,"A","left"]']},
    // début zone sprint #1
    // chance sur la case tout à droite
    '[21,"A","left"]':{position: 21, luck: true, nbBikesMax: 3, nbBikes:0, next: ['[22,"A","left"]']},
    '[22,"A","left"]':{position: 22, luck: false, nbBikesMax: 3, nbBikes:0, next: ['[23,"A","left"]','[23,"A","right"]']},
    //partie gauche
    '[23,"A","left"]':{position: 23, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[24,"A","left"]']},
    '[24,"A","left"]':{position: 24, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[25,"A","left"]']},
    '[25,"A","left"]':{position: 25, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[26,"A","left"]','[26,"B","left"]']},
    '[26,"A","left"]':{position: 26, luck: true, nbBikesMax: 1, nbBikes:0, next: ['[27,"A","left"]','[27,"B","left"]']},
    '[26,"B","left"]':{position: 26, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[27,"A","left"]','[27,"B","left"]']},
    '[27,"A","left"]':{position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[28,"A","left"]']},
    '[27,"B","left"]':{position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[28,"A","left"]']},
    '[28,"A","left"]':{position: 28, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[29,"A","left"]']},
    '[29,"A","left"]':{position: 29, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[30,"A","left"]']},
    '[30,"A","left"]':{position: 30, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[31,"A","left"]']},
    '[31,"A","left"]':{position: 31, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[32,"A","left"]']},
    '[32,"A","left"]':{position: 32, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[33,"A","left"]']},
    '[33,"A","left"]':{position: 33, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[34,"A","left"]']},
    '[34,"A","left"]':{position: 34, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[35,"A","left"]']},
    '[35,"A","left"]':{position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[36,"A","left"]']},
    // partie droite
    '[23,"A","right"]':{position: 23, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[24,"A","right"]']},
    '[24,"A","right"]':{position: 24, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[25,"A","right"]']},
    '[25,"A","right"]':{position: 25, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[26,"D","right"]']},
    '[26,"D","right"]':{position: 26, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[26,"C","right"]']},
    '[26,"C","right"]':{position: 26, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[27,"D","right"]']},
    '[27,"D","right"]':{position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[27,"C","right"]']},
    '[27,"C","right"]':{position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[28,"A","right"]']},
    '[28,"A","right"]':{position: 28, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[29,"A","right"]']},
    '[29,"A","right"]':{position: 29, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[30,"A","right"]']},
    '[30,"A","right"]':{position: 30, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[31,"A","right"]']},
    '[31,"A","right"]':{position: 31, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[32,"A","right"]']},
    '[32,"A","right"]':{position: 32, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[33,"A","right"]']},
    '[33,"A","right"]':{position: 33, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[34,"A","right"]']},
    '[34,"A","right"]':{position: 34, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[35,"A","right"]']},
    '[35,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[36,"A","left"]']},
    // fin zone sprint #1
    '[36,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[37,"A","left"]']},
    '[37,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[38,"A","left"]']},
    '[38,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[39,"A","left"]']},
    '[39,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[40,"A","left"]']},
    '[40,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[41,"A","left"]']},
    '[41,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[42,"A","left"]']},
    '[42,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[43,"A","left"]']},
    '[43,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[44,"A","left"]']},
    '[44,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[45,"A","left"]']},
    '[45,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[46,"A","left"]']},
    '[46,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[47,"A","left"]']},
    '[47,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[48,"A","left"]']},
    '[48,"A","left"]':{position: 36, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[49,"A","left"]']},
    '[49,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[50,"A","left"]']},
    '[50,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[51,"A","left"]']},
    '[51,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[52,"A","left"]']},
    '[52,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[53,"A","left"]']},
    '[53,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[54,"A","left"]']},
    '[54,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[55,"A","left"]']},
    '[55,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[56,"A","left"]']},
    '[56,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[57,"A","left"]']},
    // seulement celle de droite est une case chance (57)
    '[57,"A","left"]':{position: 36, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[58,"A","left"]']},
    '[58,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[59,"A","left"]']},
    '[59,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[60,"A","left"]']},
    '[60,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[61,"A","left"]']},
    '[61,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[62,"A","left"]']},
    '[62,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[63,"A","left"]','[63,"C","left"]']},
    '[63,"A","left"]':{position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[64,"A","left"]','[64,"C","left"]']},
    '[63,"B","left"]':{position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[64,"A","left"]','[64,"C","left"]']},
    '[63,"C","left"]':{position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[63,"B","left"]']},
    '[64,"A","left"]':{position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[65,"A","left"]']},
    '[64,"B","left"]':{position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[65,"A","left"]']},
    '[64,"C","left"]':{position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[64,"B","left"]']},
    '[65,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[66,"A","left"]']},
    // les 2 cases sont des cartes chances (66)
    '[66,"A","left"]':{position: 36, luck: true, nbBikesMax: 2, nbBikes:0, next: ['[67,"A","left"]']},
    '[67,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[68,"A","left"]']},
    '[68,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[69,"A","left"]']},
    '[69,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[70,"A","left"]']},
    '[70,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[71,"A","left"]']},
    '[71,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[72,"A","left"]']},
    '[72,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[73,"A","right"]']},
    // route à 1 avant le sprint
    '[73,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[74,"A","left"]']},
    '[74,"A","right"]':{position: 35, luck: true, nbBikesMax: 1, nbBikes:0, next: ['[75,"A","left"]']},
    '[75,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[76,"A","left"]']},
    // début zone sprint #2
    '[76,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[77,"A","right"]']},
    '[77,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[78,"A","right"]']},
    '[78,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[79,"A","right"]']},
    '[79,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[80,"A","right"]']},
    '[80,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[81,"A","right"]']},
    '[81,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[82,"A","right"]']},
    '[82,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: ['[83,"A","right"]']},
    '[83,"A","left"]':{position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: []},
    // séparation de la route en 2
    // partie gauche
    '[84,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[85,"A","left"]']},
    '[85,"A","leftt"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[86,"A","left"]']},
    '[86,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[87,"A","left"]']},
    '[87,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[88,"A","left"]']},
    '[88,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[89,"A","left"]']},
    '[89,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[90,"A","left"]']},
    '[90,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[91,"A","left"]']},
    '[91,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[92,"A","left"]']},
    '[92,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[93,"A","left"]']},
    '[93,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[94,"A","left"]']},
    '[94,"A","left"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[95,"A","left"]']},
    // partie droite
    '[84,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[85,"A","right"]']},
    '[85,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[86,"A","right"]']},
    '[86,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[87,"A","right"]']},
    '[87,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[88,"A","right"]']},
    '[88,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[89,"C","right"]']},
    '[89,"B","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[90,"C","right"]']},
    '[89,"C","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[89,"B","right"]']},
    '[90,"B","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[91,"A","right"]']},
    '[90,"C","right"]':{position: 35, luck: true, nbBikesMax: 1, nbBikes:0, next: ['[90,"B","right"]']},
    '[91,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[92,"A","right"]']},
    '[92,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[93,"A","right"]']},
    '[93,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[94,"A","right"]']},
    '[94,"A","right"]':{position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: ['[95,"A","left"]']},
    // réunion des 2
    '[95,"A","left"]':{position: 20, luck: false, nbBikesMax: 3, nbBikes:0, next: []}
    // fin du plateau, après, zone buffer (représenter ici?)
}
// let Board: BoardDictionnary={};
// Board[[1,"A","left"]]={position: 1, luck: false, nbBikesMax: 3, nbBikes:0, next: [[2,"A","left"]]};
// Board[[2,"A","left"]]={position: 2, luck: false, nbBikesMax:3, nbBikes:0, next: [[3,"A","left"]]};
// Board[[3,"A","left"]]={position: 3, luck: false, nbBikesMax:3, nbBikes:0, next: [[4,"A","left"]]};
// Board[[4,"A","left"]]={position: 4, luck: false, nbBikesMax:3, nbBikes:0, next: [[5,"A","left"]]};
// Board[[5,"A","left"]]={position: 5, luck: false, nbBikesMax:3, nbBikes:0, next: [[6,"A","left"]]};
// Board[[6,"A","left"]]={position: 6, luck: false, nbBikesMax:3, nbBikes:0, next: [[7,"A","left"]]};
// Board[[7,"A","left"]]={position: 7, luck: false, nbBikesMax:3, nbBikes:0, next: [[8,"A","left"]]};
// Board[[8,"A","left"]]={position: 8, luck: false, nbBikesMax:3, nbBikes:0, next: [[9,"A","left"],[9,"C","left"]]};
// Board[[9,"A","left"]]={position: 9, luck: true, nbBikesMax:1, nbBikes:0, next: [[10,"A","left"],[10,"C","left"]]};
// Board[[9,"B","left"]]={position: 9, luck: false, nbBikesMax:1, nbBikes:0, next: [[10,"A","left"],[10,"C","left"]]};
// Board[[9,"C","left"]]={position: 9, luck: false, nbBikesMax:1, nbBikes:0, next: [[9,"B","left"]]};
// Board[[10,"A","left"]]={position: 10, luck: true, nbBikesMax:1, nbBikes:0, next: [[11,"A","left"]]};
// Board[[10,"B","left"]]={position: 10, luck: false, nbBikesMax:1, nbBikes:0, next: [[11,"A","left"]]};
// Board[[10,"C","left"]]={position: 10, luck: false, nbBikesMax:1, nbBikes:0, next: [[10,"B","left"]]};
// Board[[11,"A","left"]]={position: 11, luck: true, nbBikesMax:2, nbBikes:0, next: [[12,"A","left"]]};
// Board[[12,"A","left"]]={position: 12, luck: true, nbBikesMax:2, nbBikes:0, next: [[13,"A","left"]]};
// Board[[13,"A","left"]]={position: 13, luck: false, nbBikesMax:2, nbBikes:0, next: [[14,"A","left"]]};
// Board[[14,"A","left"]]={position: 14, luck: false, nbBikesMax:2, nbBikes:0, next: [[15,"A","left"]]};
// // chance vrai pour la case de droite seulement (15 et 16)
// Board[[15,"A","left"]]={position: 15, luck: true, nbBikesMax:2, nbBikes:0, next: [[16,"A","left"]]};
// Board[[16,"A","left"]]={position: 16, luck: true, nbBikesMax:2, nbBikes:0, next: [[17,"A","left"]]};
// Board[[17,"A","left"]]={position: 17, luck: false, nbBikesMax:2, nbBikes:0, next: [[18,"A","left"]]};
// Board[[18,"A","left"]]={position: 18, luck: false, nbBikesMax:2, nbBikes:0, next: [[19,"A","left"]]};
// //  chance sur ceux de droite (19)
// Board[[19,"A","left"]]={position: 19, luck: true, nbBikesMax: 3, nbBikes:0, next: [[20,"A","left"]]};
// Board[[20,"A","left"]]={position: 20, luck: false, nbBikesMax: 3, nbBikes:0, next: [[21,"A","left"]]};
// // chance sur la case tout à droite
// Board[[21,"A","left"]]={position: 21, luck: true, nbBikesMax: 3, nbBikes:0, next: [[22,"A","left"]]};
// Board[[22,"A","left"]]={position: 22, luck: false, nbBikesMax: 3, nbBikes:0, next: [[23,"A","left"],[23,"A","right"]]};
// //partie gauche
// Board[[23,"A","left"]]={position: 23, luck: false, nbBikesMax: 2, nbBikes:0, next: [[24,"A","left"]]};
// Board[[24,"A","left"]]={position: 24, luck: true, nbBikesMax: 2, nbBikes:0, next: [[25,"A","left"]]};
// Board[[25,"A","left"]]={position: 25, luck: false, nbBikesMax: 2, nbBikes:0, next: [[26,"A","left"],[26,"B","left"]]};
// Board[[26,"A","left"]]={position: 26, luck: true, nbBikesMax: 1, nbBikes:0, next: [[27,"A","left"],[27,"B","left"]]};
// Board[[26,"B","left"]]={position: 26, luck: false, nbBikesMax: 1, nbBikes:0, next: [[27,"A","left"],[27,"B","left"]]};
// Board[[27,"A","left"]]={position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: [[28,"A","left"]]};
// Board[[27,"B","left"]]={position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: [[28,"A","left"]]};
// Board[[28,"A","left"]]={position: 28, luck: true, nbBikesMax: 2, nbBikes:0, next: [[29,"A","left"]]};
// Board[[29,"A","left"]]={position: 29, luck: false, nbBikesMax: 2, nbBikes:0, next: [[30,"A","left"]]};
// Board[[30,"A","left"]]={position: 30, luck: true, nbBikesMax: 2, nbBikes:0, next: [[31,"A","left"]]};
// Board[[31,"A","left"]]={position: 31, luck: false, nbBikesMax: 2, nbBikes:0, next: [[32,"A","left"]]};
// Board[[32,"A","left"]]={position: 32, luck: true, nbBikesMax: 2, nbBikes:0, next: [[33,"A","left"]]};
// Board[[33,"A","left"]]={position: 33, luck: false, nbBikesMax: 2, nbBikes:0, next: [[34,"A","left"]]};
// Board[[34,"A","left"]]={position: 34, luck: true, nbBikesMax: 2, nbBikes:0, next: [[35,"A","left"]]};
// Board[[35,"A","left"]]={position: 35, luck: false, nbBikesMax: 2, nbBikes:0, next: [[36,"A","left"]]};
// // partie droite
// Board[[23,"A","right"]]={position: 23, luck: false, nbBikesMax: 1, nbBikes:0, next: [[24,"A","right"]]};
// Board[[24,"A","right"]]={position: 24, luck: false, nbBikesMax: 1, nbBikes:0, next: [[25,"A","right"]]};
// Board[[25,"A","right"]]={position: 25, luck: false, nbBikesMax: 1, nbBikes:0, next: [[26,"D","right"]]};
// Board[[26,"D","right"]]={position: 26, luck: false, nbBikesMax: 1, nbBikes:0, next: [[26,"C","right"]]};
// Board[[26,"C","right"]]={position: 26, luck: false, nbBikesMax: 1, nbBikes:0, next: [[27,"D","right"]]};
// Board[[27,"D","right"]]={position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: [[27,"C","right"]]};
// Board[[27,"C","right"]]={position: 27, luck: false, nbBikesMax: 1, nbBikes:0, next: [[28,"A","right"]]};
// Board[[28,"A","right"]]={position: 28, luck: false, nbBikesMax: 1, nbBikes:0, next: [[29,"A","right"]]};
// Board[[29,"A","right"]]={position: 29, luck: false, nbBikesMax: 1, nbBikes:0, next: [[30,"A","right"]]};
// Board[[30,"A","right"]]={position: 30, luck: false, nbBikesMax: 1, nbBikes:0, next: [[31,"A","right"]]};
// Board[[31,"A","right"]]={position: 31, luck: false, nbBikesMax: 1, nbBikes:0, next: [[32,"A","right"]]};
// Board[[32,"A","right"]]={position: 32, luck: false, nbBikesMax: 1, nbBikes:0, next: [[33,"A","right"]]};
// Board[[33,"A","right"]]={position: 33, luck: false, nbBikesMax: 1, nbBikes:0, next: [[34,"A","right"]]};
// Board[[34,"A","right"]]={position: 34, luck: false, nbBikesMax: 1, nbBikes:0, next: [[35,"A","right"]]};
// Board[[35,"A","right"]]={position: 35, luck: false, nbBikesMax: 1, nbBikes:0, next: [[36,"A","left"]]};
// // réunion des 2
// Board[[36,"A","left"]]={position: 36, luck: false, nbBikesMax: 2, nbBikes:0, next: []};


interface Bike {
    position: number;
    reduce: number;
    turn: number;
}

interface Player {
    playerID: number;
    hand: number[];
    bikes: Bike[];
}

interface DCtx {
    deck: number[];
    discard: number[];
    turn: number;
    currentPlayer: { playerID: number, bikeIndex: number };
    players: Player[];
}

interface Ctx {
    turn: number;
    currentPlayer: string;
    numPlayers: number;
}

interface Context {
    G: DCtx;
    ctx: Ctx;
}

// Functions

/*
 * 
 * @param {number} nbCases Number of cases of the board
 * @returns The board as dictionary with the index as key and the value as the position
 */
// function defineBoard(nbCases: number): {[key: BoardIndex ]: BoardCase} {
//     // TODO: Define the board (hardcoded for now)
//     let board: {[key: BoardIndex ]: BoardCase} = {};
//     return board;
// }

/**
 * 
 * @param {Context} context Context of the game
 * @returns The playerID of the first player
 * 
 * The first player is the player who has the bike with the highest position
 */
function firstPlayer(context: Context) {
    let firstPlayer = context.G.currentPlayer;
    for (let i = 0; i < nbPlayers; i++) {
        for (let j = 0; j < nbBikes; j++) {
            if (context.G.players[i].bikes[j].position > firstPlayer.bikeIndex) {
                firstPlayer = { playerID: i, bikeIndex: j }
            }
        }
    }
    return firstPlayer.playerID;
}

/**
 *  
 *  @param {Context} context Context of the game
 *  @returns The playerID of the next player
 *  
 *  The next player is the player who has the bike with the highest position after the current player
 */ 
function nextPlayer(context: Context) {
    const firstPlayer = context.G.currentPlayer;
    let nextPlayer = context.G.currentPlayer;
    for (let i = 0; i < nbPlayers; i++) {
        for (let j = 0; j < nbBikes; j++) {
            if (context.G.players[i].bikes[j].position < context.G.players[firstPlayer.playerID].bikes[firstPlayer.bikeIndex].position && context.G.players[i].bikes[j].position > context.G.players[nextPlayer.playerID].bikes[nextPlayer.bikeIndex].position) {
                nextPlayer = { playerID: i, bikeIndex: j }
            }
        }
    }
    return nextPlayer.playerID;
}

/**
 * @param {Context} context Context of the game
 * @returns True if the game is over, false otherwise
 * 
 * The game is over if all the bikes of all the players are at the finish line
 */
function isGameOver({ G }: Context) {
    return G.players.every((player: Player) => player.bikes.every((bike: Bike) => bike.position >= nbCases));
}

/**
 * 
 * @param {Context} context Context of the game
 * @returns The ranking of the players
 * 
 * The ranking is based on the sum of the positions of the bikes of the player
 * If two players have the same sum of positions, the ranking is based on the reduce value
 * If two players have the same sum of positions and the same reduce value, the ranking is based on the sum of turns of the bikes of the player
 */
function winnerRanking({ G, ctx }: Context) {
    const sumPositionOfBikes = G.players.map(player => player.bikes.reduce((acc, bike) => acc + bike.position, 0));
    const applyReduce = G.players.map(player => player.bikes.reduce((acc, bike) => acc + bike.reduce, 0));
    const sumTurnOfBikes = G.players.map(player => player.bikes.reduce((acc, bike) => acc + bike.turn, 0));

    const ranking = G.players.map((player, index) => ({
        ...player,
        sumPosition: sumPositionOfBikes[index] - applyReduce[index],
        reduce: applyReduce[index],
        sumTurn: sumTurnOfBikes[index],
    }));

    // Sort the ranking array based on the sum of positions and the reduce value
    ranking.sort((a, b) => {
        if (a.sumPosition === b.sumPosition) {
            if (a.reduce === b.reduce)
                return a.sumTurn - b.sumTurn;
            return a.reduce - b.reduce;
        }
        return a.sumPosition - b.sumPosition;
    });

    return ranking.map(player => player.playerID);
}

/**
 * 
 *  @param {Context} context Context of the game
 *  @param {number} card Card to use
 *  @param {number} bikeIndex Index of the bike to use the card on
 * 
 *  @returns The new game state with following effects
 *  effects: delete the card from the hand of the player and add the card to the discard pile
 *  effects: move the bike of the player by the value of the card
 *  effects: if the bike is at the finish line, the bike is reduced by the overflow of the card
 * 
 *  Use the card on the bike
 */
function useCardOnBike({ G }: Context, cardIndex: number) {
    // TODO: Deep copy of G
    let myG = { ...G }; 
    const player = myG.players[G.currentPlayer.playerID];
    const card = player.hand[cardIndex];
    const bike = player.bikes[G.currentPlayer.bikeIndex];
    bike.position += card;
    if (bike.position > nbCases) {
        bike.reduce = bike.position + card - nbCases;
        if (bike.reduce > nbReduceMax) {
            bike.reduce = nbReduceMax;
        }
        bike.position = nbCases;
    }
    player.hand.splice(cardIndex, 1);
    myG.discard.push(card);
    return myG;
}

function drawCards({ G }: Context) {
    const player = G.players[G.currentPlayer.playerID];
    for (let i = 0; i < nbCardsHand; i++) {
        G.deck = shuffle(G.deck);
        let card = G.deck.pop();
        if (card !== undefined)
            player.hand.push(card);
        else
            i--;

        if (G.deck.length === 0) {
            G.deck = G.discard; // shuffled at each iteration
            G.discard = [];
        }
    }
}

const TourDeFrance = {
    setup: () => ({
        deck: shuffle(CardsDeck),
        discard: [],
        turn: 0,
        currentPlayer: {
            playerID: Math.floor(Math.random() * nbPlayers),
            bikeIndex: Math.floor(Math.random() * nbBikes),
        },
        players: [...Array(nbPlayers)].map((_, playerID) => ({ // generate each player
            playerID,
            hand: [],
            // generate each bike by player
            bikes: [...Array(nbBikes)].map(() => ({ position: 0, reduce: 0, turn: 0 })),
        })),
    }),

    players: {
        moveLimit: 3,
    },

    endIf: ({ G, ctx }: Context) => {
        if (isGameOver({ G, ctx })) {
            return { winner: winnerRanking({ G, ctx })[0] };
        }
    },

    turn: {
        order: {
            first: (context: FnContext<DCtx, Record<string, unknown>>) => firstPlayer(context),
            next: (context: FnContext<DCtx, Record<string, unknown>>) => nextPlayer(context),
        },
        minMoves: 0, // If all bike's player are at the finish line
        maxMoves: nbBikes,
    },

    phase: {
        init: {
            moves: {
                drawFirstCards: drawCards,
            },
            next: 'play',
        },
        play: {
            move: {
                useCard: (context: Context, bikeIndex: number) => {context.G = useCardOnBike(context, bikeIndex)},
                drawCards: drawCards,
            },
        },
    },
}

export { TourDeFrance, winnerRanking, useCardOnBike };
export type { Player, Bike, Context, DCtx, Ctx };