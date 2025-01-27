import * as hh from "@hop/hiphop";

const prg = hiphop module() {
   in A; in B; in R; out O;
		  
   do {
      fork {
	 await(A.now);
      } par {
	 await(B.now);
      }
      emit O(123456789);
   } every(R.now)
}

export const mach = new hh.ReactiveMachine(prg, { sweep: true, dumpNets: true, verbose: 1 });
mach.addEventListener("O", v => console.log(v));
mach.react();
mach.react({A: 1, B: 3});
