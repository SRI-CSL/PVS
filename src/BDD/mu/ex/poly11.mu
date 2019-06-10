/*
domain = { x0, w0, y0, x1, w1, y1, x2, w2, y2, x3, w3, y3,
	   x4, w4, y4, x5, w5, y5, x6, w6, y6, x7, w7, y7,
	   x8, w8, y8, x9, w9, y9, x10, w10, y10 };
*/
domain = { x10, w10, y10, x9, w9, y9, x8, w8, y8, x7, w7, y7,
	   x6, w6, y6, x5, w5, y5, x4, w4, y4, x3, w3, y3,
	   x2, w2, y2, x1, w1, y1, x0, w0, y0 };

let N = L x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
          y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10 .
          (y0 <-> x8 xor x10) &
          (y1 <-> x0) &
          (y2 <-> x1) &
          (y3 <-> x2) &
          (y4 <-> x3) &
          (y5 <-> x4) &
          (y6 <-> x5) &
          (y7 <-> x6) &
          (y8 <-> x7) &
          (y9 <-> x8) &
	  (y10 <-> x9);

/* N+ = mu Z . N + L x,y.E w. Z(x,w) & Z(w,y) */
let Nplus = mu Z . N +
            [ L x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10.
              E w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10.
                  Z(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,
                    w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10) &
                  Z(w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,
                    y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10) ];

write ("Reachable states:\n");
Nplus(1,1,1,1,1,1,1,1,1,1,1,x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10);
