const sum = require("lodash/sum");

function positiveSum(arr) {
    return sum(arr.filter(n => n > 0));
}

positiveSum([1, 2, 3, 4, 5]);
positiveSum([1, -2, 3, 4, 5]);
positiveSum([]);
positiveSum([-1, -2, -3, -4, -5]);
positiveSum([-1, 2, 3, 4, -5]);




// describe("Basic tests", () => {
//     it("Testing for fixed tests", () => {
//         assert.strictEqual(positiveSum([1, 2, 3, 4, 5]), 15);
//         assert.strictEqual(positiveSum([1, -2, 3, 4, 5]), 13);
//         assert.strictEqual(positiveSum([]), 0);
//         assert.strictEqual(positiveSum([-1, -2, -3, -4, -5]), 0);
//         assert.strictEqual(positiveSum([-1, 2, 3, 4, -5]), 9);
//     });
// });