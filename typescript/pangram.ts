// export const isPangram = (phrase: string): boolean => {

//     const alphabet = "abcdefghijklmnopqrstuvwxyz";
//     const phraseLower = phrase.toLowerCase();
//     for (let i = 0; i < alphabet.length; i++) {
//         if (phraseLower.indexOf(alphabet.charAt(i)) === -1) {
//             return false;
//         }
//     }
//     return true;
// };

export const isPangram = (phrase: string): boolean => {
    return new Set(phrase.toLowerCase()
        .replace(/[^a-z]/gi, ''))
        .size === 26;
};
// [...new Set(phrase.toLowerCase()] makes a list of unique characters

console.log(isPangram("The quick brown fox jumps over the lazy dog."), true);
console.log(isPangram("This is not a pangram."), false);