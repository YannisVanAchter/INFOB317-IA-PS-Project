
export function shuffle(array: any[]): any[] {
    // Algorithm inspired by https://stackoverflow.com/questions/2450954/how-to-randomize-shuffle-a-javascript-array/2450976#2450976

    let randomIndex: number;
    for (let currentIndex = 0; currentIndex < array.length; currentIndex++) {
        randomIndex = Math.floor(Math.random() * currentIndex);

        [array[currentIndex], array[randomIndex]] = [
            array[randomIndex], array[currentIndex]
        ];
    }

    return array;
}