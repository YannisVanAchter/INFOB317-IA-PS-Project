import { FnContext } from 'boardgame.io';
import { shuffle } from './utils/shuffle';
import { deepCopy } from './utils/deep_copy';
import { boardKey } from './utils/simonLTransform';

import { Board } from './data/board';
import { SprintsData } from './data/sprints';

import { BoardCase, Bike, Player, DCtx, Ctx, Context } from './types/game';
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
 * @param newPosition Current position of the bike
 * 
 * @returns The new position string
 */
function lucky(newSquare: string): string {
    let squareAfterLucky = newSquare;
    if (Board[newSquare].luck.length === 0) return squareAfterLucky;
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
function firstPlayer(context: Context): number {
    return parseInt(context.ctx.playOrder[0]);
}

/**
 *  
 *  @param {Context} context Context of the game
 *  @returns The playerID of the next player
 *  
 *  The next player is the player who has the bike with the highest position after the current player
 */ 
function nextPlayer(context: Context): number {
    const firstPlayer = context.ctx.currentPlayer;
    const nextPlayerIndex = context.ctx.playOrder.findIndex((playerID) => playerID === firstPlayer) + 1;
    const nextPlayer = context.ctx.playOrder[nextPlayerIndex];
    return parseInt(nextPlayer);
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
function winnerRanking({ G, ctx }: Context): {playerID: number, score: number}[] {
    let playersScore = [0, 0, 0, 0];
    let currentSecondsAllBikes = [];
    for (const player of G.players) {
        for (const bike of player.bikes) {
            if (bike.reduce === 0) {
                currentSecondsAllBikes.push({playerID: player.playerID, bike: bike, seconds: Board[bike.position].position - bike.bonusSeconds + bike.malusSeconds});
            }
        }
    }
    currentSecondsAllBikes = currentSecondsAllBikes.sort((a, b) => a.seconds - b.seconds);
    const points = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0];
    for (let i = 0; i < currentSecondsAllBikes.length; i++) {
        playersScore[currentSecondsAllBikes[i].playerID] += points[i];
    }

    for (let i = 0; i < G.players.length; i++) {
        playersScore[i] += G.players[i].bonusPoints;
    }

    if (isGameOver({G , ctx})) {
        let allPlayersTime = Array(4).map((v, i) => {return {playerID: i, time: 0}})
        for (const bike of currentSecondsAllBikes) {
            allPlayersTime[bike.playerID].time += bike.seconds;
        }
        allPlayersTime.sort((a, b) => a.time - b.time);
        const teamPoints = [40, 15, 5];
        const bikePoints = [15, 10, 5];
        for (let i = 0; i < allPlayersTime.length - 1; i++) {
            playersScore[allPlayersTime[i].playerID] += teamPoints[i];
            playersScore[currentSecondsAllBikes[i].playerID] += bikePoints[i];
        }
    }
    return Array(4).map((v, i) => {
        return {playerID: i, score: playersScore[i]};
    }).sort((a, b) => a.score - b.score);
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
        let possiblePositions = getPossibleTilesFromPosition(nbCases + 1);
        return [possiblePositions[tempPos-1]];
    }

    if (!checkMove(bike, card)) {
        return [];
    }

    let newPosition = getPossibleTilesFromPosition(numberedPosition) ?? [];
    let possiblePositions = [...newPosition];
    for (let i = 0; i < newPosition.length; i++) {
        if (checkAspiration(possiblePositions[i])) {
            possiblePositions.push(...Board[possiblePositions[i]].next);
        }
    }

    return possiblePositions;
}

/**
 * Check if a bike won or got second at a sprint and adds the points and seconds accordingly.
 * @param bike Bike that just had a turn.
 * @param player Player that just had a turn.
 */
function handleSprints(context: Context, bike: Bike, player: Player) {
    for (const sprint of SprintsData) {
        if (sprint.firstBike === undefined) {
            if (Board[bike.position].position > sprint.numberedPosition) {
                sprint.firstBike = bike;
                bike.bonusSeconds += sprint.firstGains.bonusSeconds;
                player.bonusPoints += sprint.firstBike.bonusSeconds;
            }
        } else if (sprint.secondGains !== undefined && sprint.secondBike === undefined) {
            if (Board[bike.position].position > sprint.numberedPosition) {
                sprint.secondBike = bike;
                bike.bonusSeconds += sprint.secondGains.bonusSeconds;
                player.bonusPoints += sprint.secondGains.bonusPoints;
            }
        } else if (!context.G.oneBikeFinished && bike.reduce > 0) {
            context.G.oneBikeFinished = true;
        }
    }
}

function handlePenalty(context: Context): DCtx {
    let newG = deepCopy(context.G);
    if (newG.oneBikeFinished) {
        for (const player of newG.players) {
            for (const bike of player.bikes) {
                if (bike.reduce === 0) {
                    bike.malusSeconds += 10;
                }
            }
        }
    }
    return newG;
}

/**
 * 
 *  @param {Context} context Context of the game
 *  @param {number} bikeIndex Index of the bike to move
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
function useCardOnBike(context: Context, bikeIndex: number, cardIndex: number, target: boardKey) {
    console.log("----PARAMS----");
    console.log("context: ", context);
    console.log("bike index: ", bikeIndex);
    console.log("card index: ", cardIndex);
    console.log("target: ", target);
    console.log("---- END PARAM -----");
    let myG = deepCopy(context.G); 
    const player = myG.players[parseInt(context.ctx.currentPlayer)];
    const card = player.hand[cardIndex];
    console.log(card);
    // TODO: Check which bikes can 
    const bike = player.bikes[bikeIndex];
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
        // Check si il a la possibilité de jouer autre chose qui ne provoque pas de chute
        // Sinon provoqué une chute
    }

    let possibleTiles = getPossibleTilesFromPosition(numberedPosition);
    for (const tile of possibleTiles) {
        if (checkAspiration(tile)) {
            for (let i = 0; i < Board[tile].next.length; i++) {
                possibleTiles.push(Board[tile].next[i]);
            }
        }
    }
    
    // Do the move
    console.log("card: ", card);
    console.log("numbered position: ", numberedPosition);
    console.log("possible tiles: ", possibleTiles);
    const tileIndex = possibleTiles.findIndex((val) => (val === target))
    console.log(tileIndex)
    if (tileIndex === -1) throw new Error("Invalid position required");
    let newTile = possibleTiles[tileIndex];
    bike.turn = context.ctx.turn;
    
    // Apply luck from tile selected if applicable
    newTile = lucky(newTile);

    // Put the person on the right square
    bike.position = newTile;
    player.hand.splice(cardIndex, 1);
    myG.discard.push(card);

    // Update board
    Board[oldPosition].nbBikes--;
    Board[bike.position].nbBikes++;

    // Draw cards if the player has no cards left in hand
    if (player.hand.length === 0) 
        drawCards({ G: myG, ctx: context.ctx});

    // check if sprints has been reach
    handleSprints(context, bike, player);

    return myG;
}

function setUp() {
    let ctx = {
        deck: shuffle(CardsDeck),
        discard: [],
        players: [...Array(nbPlayers)].map((_, playerID) => ({ // generate each player
            playerID,
            hand: [],
            bonusPoints: 0,
            // generate each bike by player
            bikes: [...Array(nbBikes)].map(() => ({ position: '0_B_left', reduce: 0, turn: 0, bonusSeconds: 0, malusSeconds: 0 })),
        })),
        oneBikeFinished: false
    } as DCtx;

    for (let i = 0; i < nbPlayers; i++) {
        drawCards({G: ctx, ctx: {playOrder: [], turn: 0, currentPlayer: i.toString(), numPlayers: nbPlayers}});
    }

    return ctx;
}

function bot({ G, ctx }: Context ) {
    // TODO: check with @Maragaux what AI will return
    let moves: number[] = [];
    const url = `${process.env.REACT_APP_SERVER_URL}/ai/`;
    axios.get(url, {
        params: {
            players: G.players,
            currentPlayer: { playerID: parseInt(ctx.currentPlayer) },
        }
    })
    .then(response => {
        return response.data.json();
    })
    .then((data: { moves: string[] }) => {
        let destination: boardKey[] = data.moves;
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
    })
    .catch(error => console.error(error));
    return [];
}

const TourDeFrance = {
    setup: setUp,

    players: {
        moveLimit: 1,
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
            playOrder: (context: Context) => {
                // List of 1..nbPlayers
                const basicOrder = Array.from(Array(nbPlayers).keys());
                if (context.ctx.turn < nbPlayers) {
                    // On the first turn, sort by highest card
                    basicOrder.sort((a, b) => {
                        const playerA = context.G.players[a];
                        const playerB = context.G.players[b];
                        const cardA = Math.max(...playerA.hand);
                        const cardB = Math.max(...playerB.hand);
                        return cardB - cardA;
                    });
                }
                else {
                    // After the first turn, sort by highest bike position
                    basicOrder.sort((a, b) => {
                        const playerA = context.G.players[a];
                        const playerB = context.G.players[b];
                        const bikeAPosition = Math.max(...playerA.bikes.map(bike => Board[bike.position].position));
                        const bikeBPosition = Math.max(...playerB.bikes.map(bike => Board[bike.position].position));
                        return bikeBPosition - bikeAPosition;
                    });
                }
                return basicOrder.map((playerID) => playerID.toString());
            },
        },
        minMoves: 0, // If all bike's player are at the finish line
        maxMoves: 1,
        //@ts-ignore
        onEnd: ({G, ctx, events}): DCtx => {
            G = handlePenalty({G, ctx});
            return G;
        }
    },

    events: {
        endGame: false,
    },

    moves: {
        useCard: (context: Context, bikeIndex: number, cardIndex: number, target: boardKey) => {
            context.G = useCardOnBike(context, bikeIndex, cardIndex, target); 
            return context.G;
        },
    },

    // ai: {
    //     enumerate: bot,
    // },
}

export { TourDeFrance, winnerRanking, useCardOnBike, mockUseCardOnBike, getBoardCase, bot, Board, nbPlayers };