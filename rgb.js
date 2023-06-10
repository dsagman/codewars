// The rgb function is incomplete. Complete it so that passing in
// RGB decimal values will result in a hexadecimal representation being returned.
// Valid decimal values for RGB are 0 - 255.
// Any values that fall out of that range must be rounded to the closest valid value.

// Note: Your answer should always be 6 characters long, the shorthand with 3 will not work here.

// The following are examples of expected output values:

// rgb(255, 255, 255) // returns FFFFFF
// rgb(255, 255, 300) // returns FFFFFF
// rgb(0,0,0) // returns 000000
// rgb(148, 0, 211) // returns 9400D3
const toHex = x => (x > 255) ? 'FF' : (x < 0) ? '00' : x.toString(16).toUpperCase().padStart(2, '0')

function rgb(r, g, b) {
    return [r, g, b].map(x => toHex(x)).join('');
}

console.log(rgb(173, 255, 47));

// describe("Tests", () => {
//     it("Basic Tests", () => {
//       Test.assertEquals(rgb(0, 0, 0), '000000')
//       Test.assertEquals(rgb(0, 0, -20), '000000')
//       Test.assertEquals(rgb(300,255,255), 'FFFFFF')
//       Test.assertEquals(rgb(173,255,47), 'ADFF2F')
//     });
//   });