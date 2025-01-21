import * as hh from "@hop/hiphop";

const prg = hiphop module() {
   out O1, O2;

   emit O1();
   yield;
   emit O2();
}

export const mach = new hh.ReactiveMachine(prg, { sweep: true, dumpNets: true, verbose: 1 });
mach.addEventListener("O1", v => console.log(v));
mach.addEventListener("O2", v => console.log(v));
mach.react();
