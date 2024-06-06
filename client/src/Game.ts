import { shuffle } from './utils/shuffle';
import { deepCopy } from './utils/deep_copy';

import { Board } from './data/board';
import { SprintsData } from './data/sprints';

import type { BoardCase, Bike, Player, DCtx, Ctx, Context } from './types/game';
import type { AIMove } from './types/bot';
import type { boardKey } from './types/board';

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
 * @param {string} position Position of the bike in the Board
 * @return {object} Return the Board case object
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
    console.log(Object.keys(Board));
    for (const key of Object.keys(Board)) {
        // console.log(Board[key]);
        // console.log(Board[key].position);
        // console.log(Board[key].nbBikes);
        // console.log(Board[key].nbBikesMax);
        if (Board[key].position == position && Board[key].nbBikes < Board[key].nbBikesMax) {
            possibleTiles.push(key);
        }
    }
    // console.log(possibleTiles);
    return possibleTiles;
}

function checkMove(currentBike: Bike, cardPlayed: number): boolean {
    console.log("CHECK MOVE");
    console.log(currentBike);
    console.log(cardPlayed);
    let tilesToCheck = [];
    for (let i = 0; i < cardPlayed; i++) {
        tilesToCheck.push(Board[currentBike.position].next[0]); // For now ignore secondary path will need to see how we handle it in the front
    }
    console.log(tilesToCheck);
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
 * @param {Context} context Context of the game
 * @returns True if the game is over, false otherwise
 * 
 * The game is over if all the bikes of all the players are at the finish line
 */
function isGameOver({ G, ctx }: Context) {
    return G.players.every((player: Player) => player.bikes.every((bike: Bike) => Board[bike.position].position >= nbCases)) || ctx.turn > 210;
}

function fixBoard(players: Player[]) {
    let allPositionsUsed = []
    for (const player of players) {
        for (const bike of player.bikes) {
            allPositionsUsed.push(bike.position);
        }
    }
    for (const key of Object.keys(Board)) {
        Board[key].nbBikes = 0;
    }
    for (const position of allPositionsUsed) {
        Board[position].nbBikes++;
    }
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
    console.info("==== WINNER RANKING in ====");
    for (const player of G.players) {
        for (const bike of player.bikes) {
            if (bike.reduce === 0) {
                console.log({playerID: player.playerID, bike: bike, seconds: Board[bike.position].position - bike.bonusSeconds + bike.malusSeconds});
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
        let allPlayersTime = []
        for (let i = 0; i < nbPlayers; i++) {
            allPlayersTime.push({playerID: i, time: 0})
        }
        console.log(allPlayersTime);
        for (const bike of currentSecondsAllBikes) {
            console.log(bike.playerID);
            allPlayersTime[bike.playerID].time += bike.seconds;
        }
        allPlayersTime.sort((a, b) => a.time - b.time);
        const teamPoints = [40, 15, 5];
        const bikePoints = [15, 10, 5];
        for (let i = 0; i < 3; i++) {
            playersScore[allPlayersTime[i].playerID] += teamPoints[i];
            console.log(currentSecondsAllBikes);
            playersScore[currentSecondsAllBikes[i].playerID] += bikePoints[i];
        }
    }

    const retrunArray = [];
    for (let i = 0; i < playersScore.length; i++) 
        retrunArray.push({playerID: i, score: playersScore[i]});
    retrunArray.sort((a, b) => a.score - b.score).reverse();

    console.log(retrunArray);
    console.info("==== WINNER RANKING out ====");
    return retrunArray;
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

    let allPossibleReachablePositions = [bike.position];
    let currentPos = [bike.position];
    while (!currentPos.includes("95_A_left")) {
        for (const pos of currentPos) {
            allPossibleReachablePositions.push(...Board[pos].next)
            currentPos = Board[pos].next;
        }
    }
    let valid = []
    for (const potentialPos of possiblePositions) {
        if (allPossibleReachablePositions.includes(potentialPos)) valid.push(potentialPos);
    }
    return valid;
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
 *  @modifies delete the card from the hand of the player and add the card to the discard pile
 *  @modifies move the bike of the player by the value of the card
 *  @modifies if the bike is at the finish line, the bike is reduced by the overflow of the card
 * 
 *  Use the card on the bike
 */
function useCardOnBike(context: Context, bikeIndex: number, cardIndex: number, target: boardKey) {
    let myG = deepCopy(context.G); 
    const player = myG.players[parseInt(context.ctx.currentPlayer)];
    const card = player.hand[cardIndex];
    const bike = player.bikes[bikeIndex];
    let oldPosition = bike.position;
    let numberedPosition = Board[bike.position].position + card;
    fixBoard(myG.players);
    if (numberedPosition > nbCases) {
        bike.reduce = numberedPosition + card - nbCases;
        if (bike.reduce > nbReduceMax) {
            bike.reduce = nbReduceMax;
        }
        if (numberedPosition > 105) numberedPosition = 105;
        let possiblePositions = getPossibleTilesFromPosition(numberedPosition);
        possiblePositions = [...possiblePositions, ...getPossibleTilesFromPosition(numberedPosition - 1)];
        console.log("USE CARD ON BIKE");
        console.log(possiblePositions);
        bike.position = possiblePositions[0];
        return myG;
    }
    // Check every tile on the way is clear or has space
    if (!checkMove(bike, card)) {
        // Check si il a la possibilité de jouer autre chose qui ne provoque pas de chute
        // Sinon provoqué une chute
    }
    console.log("--- USE CARD ON BIKE ---")

    console.log("bike position: " + bike.position);
    console.log("card value: " + card);

    console.log(numberedPosition)
    let possibleTiles = getPossibleTilesFromPosition(numberedPosition);
    console.log(possibleTiles);
    for (const tile of possibleTiles) {
        if (checkAspiration(tile)) {
            for (let i = 0; i < Board[tile].next.length; i++) {
                possibleTiles.push(Board[tile].next[i]);
            }
        }
    }
    
    // Do the move
    console.log(myG.players);
    console.log(Board);
    const tileIndex = possibleTiles.findIndex((val) => (val === target))
    if (tileIndex === -1) throw new Error("Invalid position required");
    let newTile = possibleTiles[tileIndex];
    bike.turn = context.ctx.turn;
    
    // Apply luck from tile selected if applicable
    newTile = lucky(newTile);

    // Put the person on the right square
    bike.position = newTile;
    player.hand.splice(cardIndex, 1);
    myG.discard.push(card);

    // Update Board
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
        drawCards({G: ctx, ctx: {playOrderPos: 0, playOrder: [], turn: 0, currentPlayer: i.toString(), numPlayers: nbPlayers}});
    }

    return ctx;
}

function getBikeIndexFromPlayerAndPos(player: Player, pos: string): number {
    let toReturn = -1;
    for (let i = 0; i < player.bikes.length; i++) {
        console.log("FROM GET BIKE INDEX");
        console.log(player.bikes[i].position);
        console.log(pos);
        
        if (player.bikes[i].position === pos) toReturn = i;
        console.log(toReturn);
    }
    console.log(toReturn);
    return toReturn;
}

function getCardIndexFromPlayerAndValue(player: Player, value: number): number {
    let toReturn = -1;
    for (let i = 0; i < player.hand.length; i++) {
        if (player.hand[i] === value) toReturn = i;
    }
    console.log("GET CARD INDEX");
    console.log(value);
    console.log(player.hand);
    console.log(toReturn);
    return toReturn;
}

function bot(G: DCtx, ctx: Ctx, playerID: string): Promise<{ bikeIndex: number, cardIndex: number, target: boardKey }> {
    // TODO: check with @Maragaux what AI will return
    let moves: number[] = [];
    const url = `http://localhost:8080/ia`;

    let currentPlayer = G.players.find(p => p.playerID === parseInt(ctx.currentPlayer));
    if (currentPlayer === undefined) throw new Error("Some real shit is happening (from bot)");
    
    let currentPlayerArray: any[] = [0];
    let currentPlayerHand = []
    for (let i = 0; i < currentPlayer.hand.length; i++) {
        currentPlayerHand.push(currentPlayer.hand[i]);
    }
    currentPlayerArray.push(currentPlayerHand);
    let currentPlayerBikePos = []
    for (let i = 0; i < currentPlayer.bikes.length; i++) {
        currentPlayerBikePos.push(currentPlayer.bikes[i].position);
    }
    currentPlayerArray.push(currentPlayerBikePos);
    // currentPlayerArray += "*";
    let otherPlayersArray = [];
    otherPlayersArray.push(currentPlayerArray);
    let currentIndexToSend = 1;
    for (const player of G.players) {
        if (player == currentPlayer) continue;
        let thisPlayer: any[] = [];
        thisPlayer.push(currentIndexToSend);
        let thisPlayerHand = []
        for (let i = 0; i < player.hand.length; i++) {
            thisPlayerHand.push(player.hand[i]);

        }
        thisPlayer.push(thisPlayerHand);
        let thisPlayerBikePos = []
        for (let i = 0; i < player.bikes.length; i++) {
            thisPlayerBikePos.push(player.bikes[i].position)
        }
        thisPlayer.push(thisPlayerBikePos);
        otherPlayersArray.push(thisPlayer);
        currentIndexToSend++;
    }
    console.log("FORMATED ARRAY DATA");
    console.log(otherPlayersArray);
    console.log("---------------------")
    let otherPlayersArrayModified = deepCopy(otherPlayersArray);
    let mainArray: any[] = [];

    for (let i = 0; i < otherPlayersArray.length; i++) {
        otherPlayersArrayModified[i][1] = otherPlayersArrayModified[i][1].join(';')
        otherPlayersArrayModified[i][2] = otherPlayersArrayModified[i][2].join(';');
        otherPlayersArrayModified[i][0] = otherPlayersArrayModified[i][0].toString();
        mainArray.push(otherPlayersArrayModified[i].join('.'))
    }

    let arrayToReturn = mainArray.join('*');

    console.log(arrayToReturn);
    return new Promise<{ bikeIndex: number, cardIndex: number, target: boardKey }>((resolve, reject) => {
        fetch(
            url, 
            {
                method: 'POST',
                // mode: 'no-cors',
                // headers: { 'Content-Type': 'application/json' },
                body: arrayToReturn
            }
        )
        .then(response => response.json())
        .then((data: {response: string}) => {
                let rawData: string[] = data.response.split(", ");
                console.log(rawData);
                let newData: AIMove = {
                    origin: rawData[2],
                    destination: rawData[1],
                    card: parseInt(rawData[0])
                };
                if (newData.card === 0) resolve({bikeIndex: 0, cardIndex: 0, target: "0_B_left"});
                console.log("NEWDATA");
                console.log(newData);
                if (currentPlayer == undefined) throw new Error("Some real shit is happening (from bot 2)");
                resolve(
                    {
                        bikeIndex: getBikeIndexFromPlayerAndPos(currentPlayer, newData.origin),
                        target: newData.destination,
                        cardIndex: getCardIndexFromPlayerAndValue(currentPlayer, newData.card)
                    }
                );
            })
            .catch(error => reject(error));
    })
}

const TourDeFrance = {
    setup: setUp,

    endIf: ({ G, ctx }: Context) => {
        if (isGameOver({ G, ctx })) {
            return { winner: winnerRanking({ G, ctx })[0] };
        }
    },

    turn: {
        order: {
            first: () => 0,
            next: ({ ctx }: Context) => (ctx.playOrderPos + 1) % ctx.numPlayers,
            playOrder: (context: Context) => {
                // List of 1..nbPlayers
                const basicOrder = Array.from(Array(nbPlayers).keys());
                if (context.ctx.turn < 1) {
                    // On the first turn, sort by highest card
                    basicOrder.sort((a, b) => {
                        const playerA = context.G.players[a];
                        const playerB = context.G.players[b];
                        const cardA = Math.max(...playerA.hand);
                        const cardB = Math.max(...playerB.hand);
                        return cardB - cardA;
                    });
                } else {
                    // After the first turn, sort by highest bike position
                    basicOrder.sort((a, b) => {
                        const playerA = context.G.players[a];
                        const playerB = context.G.players[b];
                        const bikeAPosition = Math.max(...playerA.bikes.map(bike => Board[bike.position].position));
                        const bikeBPosition = Math.max(...playerB.bikes.map(bike => Board[bike.position].position));
                        return bikeBPosition - bikeAPosition;
                    });
                }
                return basicOrder.map(playerID => playerID.toString());
            },
        },
        minMoves: 0, // If all bike's player are at the finish line
        maxMoves: 1,
        endIf: (context: Context) => {
            return context.ctx.playOrderPos === context.ctx.numPlayers - 1;
        },
        onEnd: ({ G, ctx }: Context) => {
            if (ctx.turn % nbPlayers) {
                G = handlePenalty({ G, ctx });
            }
            return G;
        },
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
}

export { TourDeFrance, winnerRanking, useCardOnBike, mockUseCardOnBike, getBoardCase, bot, Board, nbPlayers };