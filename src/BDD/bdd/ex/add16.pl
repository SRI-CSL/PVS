/* a + b */

/* 0 bit is most significant bit */

let dum =
_0 _1 _2 _3

a0 b0 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7
a8 b8 a9 b9 a10 b10 a11 b11 a12 b12 a13 b13 a14 b14 a15 b15
carryin

/*
a15 b15 a14 b14 a13 b13 a12 b12 a11 b11 a10 b10 a9 b9
a8 b8 a7 b7 a6 b6 a5 b5 a4 b4 a3 b3 a2 b2 a1 b1 a0 b0
carryin
*/
.

let cout15 = ((carryin and b15) or (carryin and a15) or (b15 and a15)).
let cout14 = ((cout15 and b14) or (cout15 and a14) or (b14 and a14)).
let cout13 = ((cout14 and b13) or (cout14 and a13) or (b13 and a13)).
let cout12 = ((cout13 and b12) or (cout13 and a12) or (b12 and a12)).
let cout11 = ((cout12 and b11) or (cout12 and a11) or (b11 and a11)).
let cout10 = ((cout11 and b10) or (cout11 and a10) or (b10 and a10)).
let cout9  = ((cout10 and b9 ) or (cout10 and a9 ) or (b9  and a9 )).
let cout8  = ((cout9  and b8 ) or (cout9  and a8 ) or (b8  and a8 )).
let cout7  = ((cout8  and b7 ) or (cout8  and a7 ) or (b7  and a7 )).
let cout6  = ((cout7  and b6 ) or (cout7  and a6 ) or (b6  and a6 )).
let cout5  = ((cout6  and b5 ) or (cout6  and a5 ) or (b5  and a5 )).
let cout4  = ((cout5  and b4 ) or (cout5  and a4 ) or (b4  and a4 )).
let cout3  = ((cout4  and b3 ) or (cout4  and a3 ) or (b3  and a3 )).
let cout2  = ((cout3  and b2 ) or (cout3  and a2 ) or (b2  and a2 )).
let cout1  = ((cout2  and b1 ) or (cout2  and a1 ) or (b1  and a1 )).
let cout0  = ((cout1  and b0 ) or (cout1  and a0 ) or (b0  and a0 )).

let o15 = ((a15 xor b15) xor carryin).
let o14 = ((a14 xor b14) xor cout15).
let o13 = ((a13 xor b13) xor cout14).
let o12 = ((a12 xor b12) xor cout13).
let o11 = ((a11 xor b11) xor cout12).
let o10 = ((a10 xor b10) xor cout11).
let o9  = ((a9  xor b9 ) xor cout10).
let o8  = ((a8  xor b8 ) xor cout9 ).
let o7  = ((a7  xor b7 ) xor cout8 ).
let o6  = ((a6  xor b6 ) xor cout7 ).
let o5  = ((a5  xor b5 ) xor cout6 ).
let o4  = ((a4  xor b4 ) xor cout5 ).
let o3  = ((a3  xor b3 ) xor cout4 ).
let o2  = ((a2  xor b2 ) xor cout3 ).
let o1  = ((a1  xor b1 ) xor cout2 ).
let o0  = ((a0  xor b0 ) xor cout1 ).

ite (_0, ite (_1, ite (_2, ite (_3, o15, o14), ite (_3, o13, o12)),
	          ite (_2, ite (_3, o11, o10), ite (_3, o9 , o8 ))),
         ite (_1, ite (_2, ite (_3, o7 , o6 ), ite (_3, o5 , o4 )),
	          ite (_2, ite (_3, o3 , o2 ), ite (_3, o1 , o0 ))))
.
