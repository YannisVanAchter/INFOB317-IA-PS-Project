import { FnContext } from 'boardgame.io';
import { shuffle } from './utils/shuffle';
import { deepCopy } from './utils/deep_copy';
import { boardKey } from './utils/simonLTransform';

import { BoardCase, dico, Bike, Player, DCtx, Ctx, Context } from './types/game';

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
const nbCardsHand = 5;
const CardsDeck = [...Array(nbCards / CardsSeconds.length)].flatMap(() => CardsSeconds); // 8x 1-12 = 96 cards

let Board: dico<BoardCase>={
    '0-B-left':{position: 0, luck: [], nbBikesMax: 20, nbBikes:0, next: ['1-A-left']}, // 0
    '1-A-left':{position: 1, luck: [], nbBikesMax: 3, nbBikes:0, next: ['2-A-left']},
    '2-A-left':{position: 2, luck: [], nbBikesMax:3, nbBikes:0, next: ['3-A-left']},
    '3-A-left':{position: 3, luck: [], nbBikesMax:3, nbBikes:0, next: ['4-A-left']},
    '4-A-left':{position: 4, luck: [], nbBikesMax:3, nbBikes:0, next: ['5-A-left']},
    '5-A-left':{position: 5, luck: [], nbBikesMax:3, nbBikes:0, next: ['6-A-left']},
    '6-A-left':{position: 6, luck: [], nbBikesMax:3, nbBikes:0, next: ['7-A-left']},
    '7-A-left':{position: 7, luck: [], nbBikesMax:3, nbBikes:0, next: ['8-A-left'], prev: '6-A-left'},
    '8-A-left':{position: 8, luck: [], nbBikesMax:3, nbBikes:0, next: ['9-A-left','9-C-left'], prev: '7-A-left'},
    '9-A-left':{position: 9, luck: [1], nbBikesMax:1, nbBikes:0, next: ['10-A-left','10-C-left'], prev: '8-A-left'},
    '9-B-left':{position: 9, luck: [], nbBikesMax:1, nbBikes:0, next: ['10-A-left','10-C-left']},
    '9-C-left':{position: 9, luck: [], nbBikesMax:1, nbBikes:0, next: ['9-B-left']},
    '10-A-left':{position: 10, luck: [1], nbBikesMax:1, nbBikes:0, next: ['11-A-left'], prev: '9-A-left'},
    '10-B-left':{position: 10, luck: [], nbBikesMax:1, nbBikes:0, next: ['11-A-left']},
    '10-C-left':{position: 10, luck: [], nbBikesMax:1, nbBikes:0, next: ['10-B-left']},
    '11-A-left':{position: 11, luck: [1], nbBikesMax:2, nbBikes:0, next: ['12-A-left'], prev: '10-A-left'},
    '12-A-left':{position: 12, luck: [1], nbBikesMax:2, nbBikes:0, next: ['13-A-left'], prev: '11-A-left'},
    '13-A-left':{position: 13, luck: [], nbBikesMax:2, nbBikes:0, next: ['14-A-left'], prev: '12-A-left'},
    '14-A-left':{position: 14, luck: [], nbBikesMax:2, nbBikes:0, next: ['15-A-left'], prev: '13-A-left'},
    '15-A-left':{position: 15, luck: [2], nbBikesMax:2, nbBikes:0, next: ['16-A-left'], prev: '14-A-left'},
    '16-A-left':{position: 16, luck: [2], nbBikesMax:2, nbBikes:0, next: ['17-A-left'], prev: '15-A-left'},
    '17-A-left':{position: 17, luck: [], nbBikesMax:2, nbBikes:0, next: ['18-A-left'], prev: '16-A-left'},
    '18-A-left':{position: 18, luck: [], nbBikesMax:2, nbBikes:0, next: ['19-A-left'], prev: '17-A-left'},
    '19-A-left':{position: 19, luck: [3], nbBikesMax: 3, nbBikes:0, next: ['20-A-left'], prev: '18-A-left'},
    '20-A-left':{position: 20, luck: [], nbBikesMax: 3, nbBikes:0, next: ['21-A-left'], prev: '19-A-left'},
    // début zone sprint #1
    '21-A-left':{position: 21, luck: [3], nbBikesMax: 3, nbBikes:0, next: ['22-A-left'], prev: '20-A-left'},
    '22-A-left':{position: 22, luck: [], nbBikesMax: 3, nbBikes:0, next: ['23-A-left','23-A-right'], prev: '21-A-left'},
    //partie gauche
    '23-A-left':{position: 23, luck: [], nbBikesMax: 2, nbBikes:0, next: ['24-A-left'], prev: '22-A-left'},
    '24-A-left':{position: 24, luck: [1], nbBikesMax: 2, nbBikes:0, next: ['25-A-left'], prev: '23-A-left'},
    '25-A-left':{position: 25, luck: [], nbBikesMax: 2, nbBikes:0, next: ['26-A-left','26-B-left'], prev: '24-A-left'},
    '26-A-left':{position: 26, luck: [1], nbBikesMax: 1, nbBikes:0, next: ['27-A-left','27-B-left'], prev: '25-A-left'},
    '26-B-left':{position: 26, luck: [], nbBikesMax: 1, nbBikes:0, next: ['27-A-left','27-B-left']},
    '27-A-left':{position: 27, luck: [], nbBikesMax: 1, nbBikes:0, next: ['28-A-left'], prev: '26-A-left'},
    '27-B-left':{position: 27, luck: [], nbBikesMax: 1, nbBikes:0, next: ['28-A-left']},
    '28-A-left':{position: 28, luck: [1], nbBikesMax: 2, nbBikes:0, next: ['29-A-left'], prev: '27-A-left'},
    '29-A-left':{position: 29, luck: [], nbBikesMax: 2, nbBikes:0, next: ['30-A-left'], prev: '28-A-left'},
    '30-A-left':{position: 30, luck: [1], nbBikesMax: 2, nbBikes:0, next: ['31-A-left'], prev: '29-A-left'},
    '31-A-left':{position: 31, luck: [], nbBikesMax: 2, nbBikes:0, next: ['32-A-left'], prev: '30-A-left'},
    '32-A-left':{position: 32, luck: [1], nbBikesMax: 2, nbBikes:0, next: ['33-A-left'], prev: '31-A-left'},
    '33-A-left':{position: 33, luck: [], nbBikesMax: 2, nbBikes:0, next: ['34-A-left'], prev: '32-A-left'},
    '34-A-left':{position: 34, luck: [1], nbBikesMax: 2, nbBikes:0, next: ['35-A-left'], prev: '33-A-left'},
    '35-A-left':{position: 35, luck: [], nbBikesMax: 2, nbBikes:0, next: ['36-A-left']},
    // partie droite
    '23-A-right':{position: 23, luck: [], nbBikesMax: 1, nbBikes:0, next: ['24-A-right']},
    '24-A-right':{position: 24, luck: [], nbBikesMax: 1, nbBikes:0, next: ['25-A-right']},
    '25-A-right':{position: 25, luck: [], nbBikesMax: 1, nbBikes:0, next: ['26-D-right']},
    '26-D-right':{position: 26, luck: [], nbBikesMax: 1, nbBikes:0, next: ['26-C-right']},
    '26-C-right':{position: 26, luck: [], nbBikesMax: 1, nbBikes:0, next: ['27-D-right']},
    '27-D-right':{position: 27, luck: [], nbBikesMax: 1, nbBikes:0, next: ['27-C-right']},
    '27-C-right':{position: 27, luck: [], nbBikesMax: 1, nbBikes:0, next: ['28-A-right']},
    '28-A-right':{position: 28, luck: [], nbBikesMax: 1, nbBikes:0, next: ['29-A-right']},
    '29-A-right':{position: 29, luck: [], nbBikesMax: 1, nbBikes:0, next: ['30-A-right']},
    '30-A-right':{position: 30, luck: [], nbBikesMax: 1, nbBikes:0, next: ['31-A-right']},
    '31-A-right':{position: 31, luck: [], nbBikesMax: 1, nbBikes:0, next: ['32-A-right']},
    '32-A-right':{position: 32, luck: [], nbBikesMax: 1, nbBikes:0, next: ['33-A-right']},
    '33-A-right':{position: 33, luck: [], nbBikesMax: 1, nbBikes:0, next: ['34-A-right']},
    '34-A-right':{position: 34, luck: [], nbBikesMax: 1, nbBikes:0, next: ['35-A-right']},
    '35-A-right':{position: 35, luck: [], nbBikesMax: 1, nbBikes:0, next: ['36-A-left']},
    // fin zone sprint #1
    '36-A-left':{position: 36, luck: [], nbBikesMax: 2, nbBikes:0, next: ['37-A-left']},
    '37-A-left':{position: 37, luck: [], nbBikesMax: 2, nbBikes:0, next: ['38-A-left']},
    '38-A-left':{position: 38, luck: [], nbBikesMax: 2, nbBikes:0, next: ['39-A-left']},
    '39-A-left':{position: 39, luck: [], nbBikesMax: 2, nbBikes:0, next: ['40-A-left']},
    '40-A-left':{position: 40, luck: [], nbBikesMax: 2, nbBikes:0, next: ['41-A-left']},
    '41-A-left':{position: 41, luck: [], nbBikesMax: 2, nbBikes:0, next: ['42-A-left']},
    '42-A-left':{position: 42, luck: [], nbBikesMax: 2, nbBikes:0, next: ['43-A-left']},
    '43-A-left':{position: 43, luck: [], nbBikesMax: 2, nbBikes:0, next: ['44-A-left']},
    '44-A-left':{position: 44, luck: [], nbBikesMax: 2, nbBikes:0, next: ['45-A-left']},
    '45-A-left':{position: 45, luck: [], nbBikesMax: 2, nbBikes:0, next: ['46-A-left']},
    '46-A-left':{position: 46, luck: [], nbBikesMax: 2, nbBikes:0, next: ['47-A-left'], prev: '45-A-left'},
    '47-A-left':{position: 47, luck: [], nbBikesMax: 2, nbBikes:0, next: ['48-A-left'], prev: '46-A-left'},
    '48-A-left':{position: 48, luck: [1], nbBikesMax: 2, nbBikes:0, next: ['49-A-left'], prev: '47-A-left'},
    '49-A-left':{position: 49, luck: [], nbBikesMax: 2, nbBikes:0, next: ['50-A-left']},
    '50-A-left':{position: 50, luck: [], nbBikesMax: 2, nbBikes:0, next: ['51-A-left']},
    '51-A-left':{position: 51, luck: [], nbBikesMax: 2, nbBikes:0, next: ['52-A-left']},
    '52-A-left':{position: 52, luck: [], nbBikesMax: 2, nbBikes:0, next: ['53-A-left']},
    '53-A-left':{position: 53, luck: [], nbBikesMax: 2, nbBikes:0, next: ['54-A-left']},
    '54-A-left':{position: 54, luck: [], nbBikesMax: 2, nbBikes:0, next: ['55-A-left']},
    '55-A-left':{position: 55, luck: [], nbBikesMax: 2, nbBikes:0, next: ['56-A-left'], prev: '54-A-left'},
    '56-A-left':{position: 56, luck: [], nbBikesMax: 2, nbBikes:0, next: ['57-A-left'], prev: '55-A-left'},
    // seulement celle de droite est une case chance (57)
    '57-A-left':{position: 57, luck: [2], nbBikesMax: 2, nbBikes:0, next: ['58-A-left'], prev: '56-A-left'},
    '58-A-left':{position: 58, luck: [], nbBikesMax: 2, nbBikes:0, next: ['59-A-left']},
    '59-A-left':{position: 59, luck: [], nbBikesMax: 2, nbBikes:0, next: ['60-A-left']},
    '60-A-left':{position: 60, luck: [], nbBikesMax: 2, nbBikes:0, next: ['61-A-left']},
    '61-A-left':{position: 61, luck: [], nbBikesMax: 2, nbBikes:0, next: ['62-A-left']},
    '62-A-left':{position: 62, luck: [], nbBikesMax: 2, nbBikes:0, next: ['63-A-left','63-C-left']},
    '63-A-left':{position: 63, luck: [], nbBikesMax: 1, nbBikes:0, next: ['64-A-left','64-C-left']},
    '63-B-left':{position: 63, luck: [], nbBikesMax: 1, nbBikes:0, next: ['64-A-left','64-C-left']},
    '63-C-left':{position: 63, luck: [], nbBikesMax: 1, nbBikes:0, next: ['63-B-left']},
    '64-A-left':{position: 64, luck: [], nbBikesMax: 1, nbBikes:0, next: ['65-A-left'], prev: '63-A-left'},
    '64-B-left':{position: 64, luck: [], nbBikesMax: 1, nbBikes:0, next: ['65-A-left']},
    '64-C-left':{position: 64, luck: [], nbBikesMax: 1, nbBikes:0, next: ['64-B-left']},
    '65-A-left':{position: 65, luck: [], nbBikesMax: 2, nbBikes:0, next: ['66-A-left'], prev: '64-A-left'},
    // les 2 cases sont des cartes chances (66)
    '66-A-left':{position: 66, luck: [1,2], nbBikesMax: 2, nbBikes:0, next: ['67-A-left'], prev: '65-A-left'},
    '67-A-left':{position: 67, luck: [], nbBikesMax: 2, nbBikes:0, next: ['68-A-left']},
    '68-A-left':{position: 68, luck: [], nbBikesMax: 2, nbBikes:0, next: ['69-A-left']},
    '69-A-left':{position: 69, luck: [], nbBikesMax: 2, nbBikes:0, next: ['70-A-left']},
    '70-A-left':{position: 70, luck: [], nbBikesMax: 2, nbBikes:0, next: ['71-A-left']},
    '71-A-left':{position: 71, luck: [], nbBikesMax: 2, nbBikes:0, next: ['72-A-left']},
    '72-A-left':{position: 72, luck: [], nbBikesMax: 2, nbBikes:0, next: ['73-A-left'], prev: '71-A-left'},
    // route à 1 avant le sprint
    '73-A-left':{position: 73, luck: [], nbBikesMax: 1, nbBikes:0, next: ['74-A-left'], prev: '72-A-left'},
    '74-A-left':{position: 74, luck: [1], nbBikesMax: 1, nbBikes:0, next: ['75-A-left'], prev: '73-A-left'},
    '75-A-left':{position: 75, luck: [], nbBikesMax: 1, nbBikes:0, next: ['76-A-left']},
    // début zone sprint #2
    '76-A-left':{position: 76, luck: [], nbBikesMax: 2, nbBikes:0, next: ['77-A-left']},
    '77-A-left':{position: 77, luck: [], nbBikesMax: 2, nbBikes:0, next: ['78-A-left']},
    '78-A-left':{position: 78, luck: [], nbBikesMax: 2, nbBikes:0, next: ['79-A-left']},
    '79-A-left':{position: 79, luck: [], nbBikesMax: 2, nbBikes:0, next: ['80-A-left']},
    '80-A-left':{position: 80, luck: [], nbBikesMax: 2, nbBikes:0, next: ['81-A-left']},
    '81-A-left':{position: 81, luck: [], nbBikesMax: 2, nbBikes:0, next: ['82-A-left']},
    '82-A-left':{position: 82, luck: [], nbBikesMax: 2, nbBikes:0, next: ['83-A-left']},
    '83-A-left':{position: 83, luck: [], nbBikesMax: 2, nbBikes:0, next: ['84-A-left','84-A-right']},
    // séparation de la route en 2
    // partie gauche
    '84-A-left':{position: 84, luck: [], nbBikesMax: 1, nbBikes:0, next: ['85-A-left']},
    '85-A-left':{position: 85, luck: [], nbBikesMax: 1, nbBikes:0, next: ['86-A-left']},
    '86-A-left':{position: 86, luck: [], nbBikesMax: 1, nbBikes:0, next: ['87-A-left']},
    '87-A-left':{position: 87, luck: [], nbBikesMax: 1, nbBikes:0, next: ['88-A-left']},
    '88-A-left':{position: 88, luck: [], nbBikesMax: 1, nbBikes:0, next: ['89-A-left']},
    '89-A-left':{position: 89, luck: [], nbBikesMax: 1, nbBikes:0, next: ['90-A-left']},
    '90-A-left':{position: 90, luck: [], nbBikesMax: 1, nbBikes:0, next: ['91-A-left']},
    '91-A-left':{position: 91, luck: [], nbBikesMax: 1, nbBikes:0, next: ['92-A-left']},
    '92-A-left':{position: 92, luck: [], nbBikesMax: 1, nbBikes:0, next: ['93-A-left']},
    '93-A-left':{position: 93, luck: [], nbBikesMax: 1, nbBikes:0, next: ['94-A-left']},
    '94-A-left':{position: 94, luck: [], nbBikesMax: 1, nbBikes:0, next: ['95-A-left']},
    // partie droite
    '84-A-right':{position: 84, luck: [], nbBikesMax: 1, nbBikes:0, next: ['85-A-right']},
    '85-A-right':{position: 85, luck: [], nbBikesMax: 1, nbBikes:0, next: ['86-A-right']},
    '86-A-right':{position: 86, luck: [], nbBikesMax: 1, nbBikes:0, next: ['87-A-right']},
    '87-A-right':{position: 87, luck: [], nbBikesMax: 1, nbBikes:0, next: ['88-A-right']},
    '88-A-right':{position: 88, luck: [], nbBikesMax: 1, nbBikes:0, next: ['89-C-right']},
    '89-B-right':{position: 89, luck: [], nbBikesMax: 1, nbBikes:0, next: ['90-C-right'], prev: '89-C-right'},
    '89-C-right':{position: 89, luck: [], nbBikesMax: 1, nbBikes:0, next: ['89-B-right'], prev: '88-A-right'},
    '90-B-right':{position: 90, luck: [], nbBikesMax: 1, nbBikes:0, next: ['91-A-right']},
    '90-C-right':{position: 90, luck: [1], nbBikesMax: 1, nbBikes:0, next: ['90-B-right'], prev: '89-B-right'},
    '91-A-right':{position: 91, luck: [], nbBikesMax: 1, nbBikes:0, next: ['92-A-right']},
    '92-A-right':{position: 92, luck: [], nbBikesMax: 1, nbBikes:0, next: ['93-A-right']},
    '93-A-right':{position: 93, luck: [], nbBikesMax: 1, nbBikes:0, next: ['94-A-right']},
    '94-A-right':{position: 94, luck: [], nbBikesMax: 1, nbBikes:0, next: ['95-A-left']},
    // réunion des 2
    '95-A-left':{position: 95, luck: [], nbBikesMax: 3, nbBikes:0, next: ['0-A-left']},
    //zone buffer
    '0-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-1-A-left']},
    '-1-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-2-A-left']},
    '-2-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-3-A-left']},
    '-3-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-4-A-left']},
    '-4-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-5-A-left']},
    '-5-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-6-A-left']},
    '-6-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-7-A-left']},
    '-7-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-8-A-left']},
    '-8-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: ['-9-A-left']},
    '-9-A-left':{position: 96, luck: [], nbBikesMax: 3, nbBikes:0, next: []}
}

// Functions
/**
 * @param {string} position Position of the bike in the board
 * @return {object} Return the board case object
 */
function getBoardCase(position: string): BoardCase {
    return Board[position];
};

/**
 * Convert the numbered position to the key for Board dict
 * @param position Position to convert
 */
function getPossibleTilesFromPosition(position: number): boardKey[] {
    // console.log(position);
    let possibleTiles: string[] = [];
    for (const key in Board) {
        // console.log(Board[key]);
        // console.log(Board[key].position);
        // console.log(Board[key].nbBikes);
        // console.log(Board[key].nbBikesMax);
        if (Board[key].position === position && Board[key].nbBikes < Board[key].nbBikesMax) {
            possibleTiles.push(key);
        }
    }
    // console.log(possibleTiles);
    return possibleTiles;
}

function checkMove(currentBike: Bike, cardPlayed: number): boolean {
    let tilesToCheck = [];
    for (let i = 0; i < cardPlayed; i++) {
        tilesToCheck.push(Board[currentBike.position].next[0]); // For now ignore secondary path will need to see how we handle it in the front
    }
    for (const tiles of tilesToCheck) {
        if (Board[tiles].nbBikes >= Board[tiles].nbBikesMax) return false;
    }
    return true;
}

/**
 * 
 * @param currentBike Current bike being moved
 * @param newPosition Current position of the bike
 * 
 * @returns The new position string
 */
function lucky(currentBike: Bike, newSquare: string): string {
    let squareAfterLucky = newSquare;
    if (Board[newSquare].luck.length === 0) return newSquare;
    if (Board[newSquare].luck.length > 0) {
        for (let i = 0; i < Board[newSquare].luck.length; i++) {
            // Check if current square is gonna be a lucky square
            if (Board[newSquare].luck[i] === (Board[newSquare].nbBikes - 1)) {
                let n = Math.floor(Math.random() * (6 + 1) - 3);
                if (n === 0) return squareAfterLucky;
                if (n > 0) {
                    for (let j = 0; j < n; j++) {
                        squareAfterLucky = Board[squareAfterLucky].next[0];
                    }
                } else {
                    for (let j = 0; j < n; j++) {
                        if (Board[squareAfterLucky].prev !== undefined) {
                            // @ts-ignore
                            squareAfterLucky = Board[squareAfterLucky].prev
                        }
                    }
                }
            }
        }
    }
    return squareAfterLucky;
}

/**
 * Check if the current player benefits from aspiration
 * @param newPosition Normal new position of the player after this turn
 * 
 * @return boolean Return if aspiration is allowed
 */
function checkAspiration(newPosition: string): boolean {
    // Check if next is someone
    // Take first before need to see in the front how we handle it
    let nextPlace = Board[newPosition].next[0]
    if (Board[nextPlace].nbBikes > 0 && Board[nextPlace].nbBikes < Board[nextPlace].nbBikesMax) return true;

    let nextNextPlace = Board[nextPlace].next[0];
    if (Board[nextNextPlace].nbBikes > 0 && Board[nextPlace].nbBikes === 0) return true;
    return false;
    // Check if second next is someone
}

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
            if (Board[context.G.players[i].bikes[j].position].position > firstPlayer.bikeIndex) {
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
    return G.players.every((player: Player) => player.bikes.every((bike: Bike) => Board[bike.position].position >= nbCases));
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
    const sumPositionOfBikes = G.players.map(player => player.bikes.reduce((acc, bike) => acc + Board[bike.position].position, 0));
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
 *  @param { G: DCtx, ctx: Ctx } context Context of the game
 *  @effects Draw cards from the deck and add them to the hand of the player 
 */
function drawCards({ G, ctx }: Context) {
    let playerID = parseInt(ctx.currentPlayer);
    const player = G.players[playerID];
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

function mockUseCardOnBike(bike: Bike, card: number): boardKey[] {
    let numberedPosition = Board[bike.position].position + card;
    if (numberedPosition > nbCases) {
        let tempPos = numberedPosition + card - nbCases;
        if (tempPos > nbReduceMax) {
            tempPos = nbReduceMax;
        }
        const possiblePositions = getPossibleTilesFromPosition(nbCases + 1);
        return [possiblePositions[tempPos-1]];
    }

    if (!checkMove(bike, card)) {
        return [];
    }

    let newPosition = getPossibleTilesFromPosition(numberedPosition);
    let possiblePositions = [...newPosition];
    for (let i = 0; i < newPosition.length; i++) {
        if (checkAspiration(possiblePositions[i])) {
            possiblePositions.push(...Board[possiblePositions[i]].next);
        }
    }

    return possiblePositions;
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
function useCardOnBike({ G, ctx }: Context, cardIndex: number) {
    let myG = deepCopy(G); 
    const player = myG.players[G.currentPlayer.playerID];
    const card = player.hand[cardIndex];
    // console.log(card);
    const bike = player.bikes[G.currentPlayer.bikeIndex];
    let oldPosition = bike.position;
    let numberedPosition = Board[bike.position].position + card;
    if (numberedPosition > nbCases) {
        bike.reduce = numberedPosition + card - nbCases;
        if (bike.reduce > nbReduceMax) {
            bike.reduce = nbReduceMax;
        }
        const possiblePositions = getPossibleTilesFromPosition(nbCases + 1);
        bike.position = possiblePositions[bike.reduce-1];
    }
    // Check every tile on the way is clear or has space
    if (!checkMove(bike, card)) {
        // Move invalide, a voir comment si on les autorisent, dans tous les cas chutent si joué
    }
    
    // Check aspiration
    // console.log(numberedPosition);
    let test = getPossibleTilesFromPosition(numberedPosition);
    // console.log(test);
    if (checkAspiration(test[0])) { // Select first possible position, check in front how to handle
        // Aspiration is allowed
    } 

    // Needs to choose which tile here, have to be done in front, for now default to the first possible one
    const possibleTiles = getPossibleTilesFromPosition(numberedPosition);
    let newTile = lucky(bike, possibleTiles[0]);

    // Put the person on the right square
    bike.position = newTile;
    player.hand.splice(cardIndex, 1);
    myG.discard.push(card);

    // Update board
    Board[oldPosition].nbBikes--;
    Board[bike.position].nbBikes++;

    // Draw cards if the player has no cards left in hand
    if (player.hand.length === 0) 
        drawCards({ G: myG, ctx});

    return myG;
}

function setUp() {
    let ctx = {
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
            bikes: [...Array(nbBikes)].map(() => ({ position: '0-B-left', reduce: 0, turn: 0 })),
        })),
    };

    for (let i = 0; i < nbPlayers; i++) {
        drawCards({G: ctx, ctx: {turn: 0, currentPlayer: i.toString(), numPlayers: nbPlayers}});
    }

    return ctx;
}

const TourDeFrance = {
    setup: setUp,

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

    moves: {
        useCard: (context: any, bikeIndex: number) => {
            // console.log(context);
            // console.log(bikeIndex);
            context.G = useCardOnBike(context, bikeIndex)},
    },
}

export { TourDeFrance, winnerRanking, useCardOnBike, mockUseCardOnBike, getBoardCase, Board };