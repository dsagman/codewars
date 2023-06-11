// export class G964 {
//     public static nbDig(n: number, d: number): number {
        
//         let squares = [];
//         for (let i = 0; i <= n; i++) {
//             squares.push(i * i);
//         }
//         let count = 0;
//         for (let i = 0; i < squares.length; i++) {  
//             count += squares[i].toString().split(d.toString()).length - 1;
//         }
//         return count;

//     }
// }

export class G964 {
    public static nbDig(n: number, d: number) {
      return Array(n + 1)
        .fill(0)
        .map((_, i) => i)
        .map(v => v ** 2)
        // .filter(v => v.toString().indexOf(d.toString()))
        .join('')
        .split('')
        .filter(c => c === d.toString())
        .length;
    }
}
console.log(G964.nbDig(5750, 0), 4700)
console.log(G964.nbDig(11011, 2), 9481)
console.log(G964.nbDig(12224, 8), 7733)
console.log(G964.nbDig(11549, 1), 11905)