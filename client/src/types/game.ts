import { Events } from "boardgame.io/dist/types/src/plugins/events/events";
import { boardKey } from "./board";

type playerID = 0 | 1 | 2 | 3;

type PlayerRep = {
    [index: number]: {
        teamName: string,
        flag: string, // Emoticon used to represent the player on the board
    }
}

type CardValue = -3 | -2 | -1 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12;

interface BoardCase {
    position: number;
    letter?: "A"|"B"|"C"|"D"; // Letter of the case (A, B, C or D)
    next: Array<string>; // Index of the next cases
    prev?: string; // Index of the previous cases, only defined around lucky square
    luck: Array<number>; // Draw luck card  , if case on the left, value is 1, and we increment going to the right
    side?: "intern"|"extern"; // Side of the case (dans le sens de la course)
    nbBikesMax: number; // Number of bikes that can be on the case
    nbBikes: number; // Number of bikes on the case (default 0)
}

// type BoardDictionnary= Record<BoardIndex, BoardCase>;
// type dico={[key: BoardIndex ]: BoardCase};
type dico<T>={
    [key: boardKey]:T;
}

interface SprintGains {
    bonusPoints: number;
    bonusSeconds: number;
}

interface Sprints {
    numberedPosition: number;
    firstGains: SprintGains;
    secondGains?: SprintGains;
    firstBike?: Bike;
    secondBike?: Bike;
}

interface Bike {
    position: string;
    reduce: number;
    turn: number;
    bonusSeconds: number;
    malusSeconds: number;
}

interface Player {
    playerID: playerID
    hand: CardValue[];
    bikes: Bike[];
    bonusPoints: number;
}

interface DCtx {
    deck: CardValue[];
    discard: CardValue[];
    players: Player[];
    oneBikeFinished: boolean;
}

interface Ctx {
    turn: number;
    currentPlayer: string;
    numPlayers: number;
    gameover?: any;
    playOrder: string[];
    playOrderPos: number;
}

interface Context {
    G: DCtx;
    ctx: Ctx;
    events?: any;
}

interface GameContextType {
    currentBikeIndex: number;
    currentCardIndex: number;
    setBikeIndex: (nexIndex: number) => void;
    handleChoiceCard: (nexIndex: number) => void;
    mockUseCard: () => boardKey[];
    applyCardOnBike: (target: boardKey) => boolean;
}

export type { BoardCase, dico, Bike, Player, DCtx, Ctx, Context, playerID, PlayerRep, CardValue, GameContextType, Sprints };