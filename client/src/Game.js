import { shuffle } from './utils/shuffle';

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
]

const nbCases = 95
const nbBikes = 3
const nbPlayers = 4
const nbCards = 96

const nbCardsFirsthand = 5;
const nbCardsHand = 3;

const CardsDeck = [...Array(nbCards/CardsSeconds.length)].flatMap(() => CardsSeconds); // 8x 1-12 = 96 cards

/**
 * 
 * @param {*} G Context of the game
 * @returns True if the game is over
 * 
 * The game is over when All players have all bike at the finish line
 */
function isGameOver({G}) {
    return G.players.every(player => player.bikes.every(bike => bike.position >= nbCases));
}

/**
 * @param {*} G Context of the game
 * @returns The ranking of the players
 * 
 * The ranking is based on the sum of the position of the bikes of each player (the lower the better)
 * An avantage is given to the players who have the lowest sum of position
 */
function winnerRanking({G, ctx}) {
    const sumPositionOfBikes = G.players.map(player => player.bikes.reduce((acc, bike) => acc + bike.position, 0));
    const applyReduce = G.players.map(player => player.bikes.reduce((acc, bike) => acc + bike.reduce, 0));

    const ranking = G.players.map((player, index) => ({
        playerID: player.playerID,
        sumPosition: sumPositionOfBikes[index] - applyReduce[index],
    }));

    // Sort the ranking array based on the sum of positions and the reduce value
    ranking.sort((a, b) => {
        if (a.sumPosition === b.sumPosition) {
            return a.reduce - b.reduce;
        }
        return a.sumPosition - b.sumPosition;
    });

    return ranking[0].playerID;
}

export const TourDeFrance = {
    setup: () => ({
        deck: shuffle(CardsDeck),
        discard: [],
        turn: 0,
        players: [...Array(nbPlayers)].map((_, playerID) => ({ // generate each player
            playerID,
            hand: [],
            // generate each bike by player
            bikes: [...Array(nbBikes)].map(() => ({ position: 0, reduce: 0 })),
        })),
    }),

    players: {
        moveLimit: 1,
    },

    endIf: (G, ctx) => {
        if (isGameOver({G})) {
            return { winner: winnerRanking({G, ctx})};
        }
    },

    move: {
        useCardOnBike: ({ G, ctx, playerID, cardIndex, bikeIndex }) => {
            const player = G.players[playerID];
            const card = player.hand[cardIndex];
            const bike = player.bikes[bikeIndex];
            if (bike.position + card > nbCases) {
                bike.reduce = bike.position + card - nbCases;
                bike.position = nbCases;
            }
            else {
                bike.position += card;
            }
            player.hand.splice(cardIndex, 1);
            G.discard.push(card);
        },

        takeFirstCards: ({ G, ctx, playerID }) => {
            const player = G.players[playerID];
            for (let i = 0; i < nbCardsFirsthand; i++) {
                player.hand.push(G.deck.pop());
            }
        },

        takeCards: ({ G, ctx, playerID }) => {
            const player = G.players[playerID];
            for (let i = 0; i < nbCardsHand; i++) {
                player.hand.push(G.deck.pop());
            }
        }
    }
}