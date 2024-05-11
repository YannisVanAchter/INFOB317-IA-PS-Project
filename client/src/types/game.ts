import { boardKey } from "./board";

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
type dico<BoardCase>={
    [key: boardKey]:BoardCase;
}



interface Bike {
    position: string;
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

export type { BoardCase, dico, Bike, Player, DCtx, Ctx, Context };