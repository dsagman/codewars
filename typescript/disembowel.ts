export class Kata {
    static disemvowel(str: string): string {
      str = str.replace(/[aeiou]/gi, '');
      return str;
    }
  }


  console.log(Kata.disemvowel("hello"));


