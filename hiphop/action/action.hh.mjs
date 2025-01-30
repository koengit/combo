import * as hh from "@hop/hiphop";

const prg = hiphop module() {
   in x;
   out y;
   signal x1, x2, y1, y2;

   fork {
      if (x.now) {
	 emit x1();
      } else {
	 if (y2.now) {
	    emit x1();
	 }
      }
   } par {
      if (x1.now) {
	 pragma { console.log("hello"); }
	 emit y1();
      }
   } par {
      if (x.now) {
	 if (y1.now) {
	    emit x2();
	 }
      } else {
	 emit x2();
      }
   } par {
      if (x2.now) {
	 pragma { console.log("goodbye"); }
	 emit y2();
      }
   } par {
      if (x.now) {
	 if (y2.now) {
	    emit y("y2");
	 }
      } else {
	 if (y1.now) {
	    emit y("y1");
	 }
      }
   }
}

export const mach = new hh.ReactiveMachine(prg, { sweep: true, dumpNets: true, verbose: 2 });

mach.addEventListener("y", v => console.log("got y", v.nowval));


mach.react({x: 1});
