"use @hop/hiphop";
"use hopscript";

import * as hh from "@hop/hiphop";

hiphop module prg() {
   signal V_S_C, V_S_i = 0;

   if (V_S_C.nowval > 0) {
      pragma { console.log("V_S_C.nowval > 0"); }
      ;
   }
   if (V_S_i.nowval > 0) {
      pragma { console.log("V_S_i.nowval > 0"); }
      emit V_S_C(0);
   }
}

export const mach = new hh.ReactiveMachine(prg, { sweep: true, dumpNets: true, verbose: 1 });
mach.outbuf = "";

try {
    mach.react();
} catch(e) {
    mach.outbuf += ("causality error") + "\n";
}
