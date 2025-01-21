"use @hop/hiphop";
"use hopscript";

import * as hh from "@hop/hiphop";
import { format } from "util";

hiphop module prg() {
   inout A, B, C;
   
   fork {
      loop {
	 if (B.nowval > 3) {
	    emit A();
	 }
	 yield;
      }
   } par {
      loop {
	 if (C.now) {
	    emit B(4);
	 }
	 yield;
      }
   }
}

export const mach = new hh.ReactiveMachine(prg, { sweep: true, dumpNets: true, verbose: 1 });
mach.addEventListener("A", v => console.log(v.signame));
mach.addEventListener("B", v => console.log(v.signame));
mach.addEventListener("C", v => console.log(v.signame));

mach.react();
mach.react({C: true});

