/* a = b */

/* 0 bit is most significant bit */

let dum =

a0 b0 a1 b1 a2 b2 a3 b3 a4 b4 a5 b5 a6 b6 a7 b7

/*
a7 b7 a6 b6 a5 b5 a4 b4 a3 b3 a2 b2 a1 b1 a0 b0

*/
/* Bad ordering: */
/*
a0 a1 a2 a3 a4 a5 a6 a7
b0 b1 b2 b3 b4 b5 b6 b7
*/
.

(a0 == b0)
(a1 == b1)
(a2 == b2)
(a3 == b3)
(a4 == b4)
(a5 == b5)
(a6 == b6)
(a7 == b7)
.
