// Link: https://www.codewars.com/kata/54da539698b8a2ad76000228/train/typescript

export function isValidWalk(walk: string[]) {
    return walk.length === 10 && walk.filter(x => x === 'n').length === walk.filter(x => x === 's').length && walk.filter(x => x === 'e').length === walk.filter(x => x === 'w').length;
}

// TODO Add your tests here

  
console.log(isValidWalk(['n','s','n','s','n','s','n','s','n','s']), true);
console.log(isValidWalk(['w','e','w','e','w','e','w','e','w','e','w','e']), false);
console.log(isValidWalk(['w']), false);
console.log(isValidWalk(['n','n','n','s','n','s','n','s','n','s']), false);


