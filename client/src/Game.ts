import { FnContext } from 'boardgame.io';
import { shuffle } from './utils/shuffle';
import { deepCopy } from './utils/deep_copy';
import { boardKey } from './utils/simonLTransform';

import { Board } from './data/board';

import { BoardCase, dico, Bike, Player, DCtx, Ctx, Context } from './types/game';
import axios from 'axios';

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
 *  @param {number} cardIndex Index of the Card to use
 *  @param {number} target Target position the player wants to get to
 * 
 * 
 *  @returns The new game state with following effects
 *  effects: delete the card from the hand of the player and add the card to the discard pile
 *  effects: move the bike of the player by the value of the card
 *  effects: if the bike is at the finish line, the bike is reduced by the overflow of the card
 * 
 *  Use the card on the bike
 */
function useCardOnBike({ G, ctx }: Context, cardIndex: number, target: string) {
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
    } as DCtx;

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

    ai: {
        enumerate: async (G: DCtx, ctx: Ctx) => {
            // TODO: check with @Maragaux what AI will return
            let moves: number[] = [];
            const url = `http://localhost:8000/ai`;
            const response = await axios.get(url, {
                params: {
                    G: G,
                    ctx: ctx,
                }
            });

            let destination: boardKey[] = response.data.moves;
            // TODO: delete this loop if return only one move
            for (let i = 0; i < destination.length; i++) {
                // TODO: Depending on the return of the AI, we will have to change this loop definition
                for (let j = 0; j < G.players[parseInt(ctx.currentPlayer)].hand.length; j++) {
                    const availableMoves = mockUseCardOnBike(G.players[parseInt(ctx.currentPlayer)].bikes[parseInt(ctx.currentPlayer)], G.players[parseInt(ctx.currentPlayer)].hand[j]);
                    if (availableMoves.includes(destination[i])) {
                        moves.push(j);
                    }
                }
                if (moves.length > 0) {
                    break;
                }
            }

            // Remove duplicates and sort in ascending order (for descending add " * (-1)" in the sort function)
            moves = [...new Set(moves)].sort((a, b) => {return (a - b)});

            return moves;
        },
    },
}

export { TourDeFrance, winnerRanking, useCardOnBike, mockUseCardOnBike, getBoardCase, Board };