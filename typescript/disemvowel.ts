// Link: https://www.codewars.com/kata/52fba66badcd10859f00097e/train/typescript


export class Kata {
    static disemvowel(str: string): string {
      str = str.replace(/[aeiou]/gi, '');
      return str;
    }
  }


  console.log(Kata.disemvowel("hello"));


