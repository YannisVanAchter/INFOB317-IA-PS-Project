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


// Types
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
                drawFirstCards: ({ G }: Context) => {
                    G.players.forEach((_, playerID) => {
                        for (let i = 0; i < nbCardsFirsthand; i++) {
                            G.deck = shuffle(G.deck);
                            const card = G.deck.pop();
                            if (card !== undefined) {
                                G.players[playerID].hand.push(card);
                            }
                        }
                    });
                },
            },
            next: 'play',
        },
        play: {
            move: {
                useCardOnBike: ({ G }: Context, cardIndex: number) => {
                    const player = G.players[G.currentPlayer.playerID];
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
                    G.discard.push(card);
                },

                drawCards: ({ G }: Context) => {
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
            },
        }
    },
}

export { TourDeFrance, winnerRanking };
export type { Player, Bike, Context, DCtx, Ctx };