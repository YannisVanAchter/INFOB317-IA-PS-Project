
/**
 * Shuffles an array in place.
 * 
 * @param array The array to shuffle.
 * @returns The shuffled array.
 */
export function shuffle(array: any[]): any[] {
    // Algorithm inspired by https://stackoverflow.com/questions/2450954/how-to-randomize-shuffle-a-javascript-array/2450976#2450976
    let shuffledArray = [...array];
    let randomIndex: number;
    for (let currentIndex = 0; currentIndex < shuffledArray.length; currentIndex++) {
        randomIndex = Math.floor(Math.random() * currentIndex);

        [shuffledArray[currentIndex], shuffledArray[randomIndex]] = [
            shuffledArray[randomIndex], shuffledArray[currentIndex]
        ];
    }

    return shuffledArray;
}