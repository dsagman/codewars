function humanReadable(seconds) {
    let hrs = Math.floor(seconds/ 3600);
    let mins = Math.floor((seconds-hrs*3600)/ 60);
    let secs = seconds-mins*60-hrs*3600;
    return [String(hrs).padStart(2,'0'),String(mins).padStart(2,'0'),String(secs).padStart(2,'0')].join(':');
  }


console.log(humanReadable(3653));
console.log("done");
