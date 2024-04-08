import React from 'react';

export function Board({ G, ctx, moves, playerID}) {
    return <>
        Board
        {G}
        {ctx}
        {moves}
        {playerID}
    </>
}