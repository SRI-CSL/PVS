let N3 = (A[1]).
let N4 = (A[3]).
let N5 = (A[2]).
let N6 = (A[4]).
let N7 = (not CARRYIN).
let N8 = (B[3]).
let N9 = (B[1]).
let N10 = (B[2]).
let N11 = (B[4]).
let N17 = NOT N3.
let N31 = NOT N4.
let N29 = NOT N5.
let N19 = NOT N7 OR NOT N7.
let N43 = NOT N6.
let N20 = NOT N19.
let N18 = (NOT ((N9 AND NOT N3) OR (NOT N9 AND N3))).
let N28 = (NOT ((N10 AND (NOT N5)) OR ((NOT N10) AND N5))).
let N32 = (NOT ((N8 AND (NOT N4)) OR ((NOT N8) AND N4))) .
let N16 = NOT N18.
let N24 = NOT N28.
let N22 = NOT N16.
let N42 = (NOT ((N11 AND (NOT N6)) OR ((NOT N11) AND N6))) .
let N38 = NOT N42.
let N27 = NOT N24.
let N21 = (NOT ((N20 AND N16) OR ((NOT N20) AND (NOT N16)))) .
let N23 = (((NOT N16) AND (NOT N3)) OR ((NOT N22) AND (NOT N19))) .
let N25 = NOT N23.
let N26 = (NOT ((N25 AND (NOT  N24)) OR ((NOT N25) AND N24))) .
let N13 = NOT N26.
let N30 = NOT N32.
let N33 = (((NOT N27) AND (NOT N23)) OR ((NOT N29) AND (NOT N24))) .
let N36 = NOT N30.
let N15 = NOT N21.
let N34 = NOT N33.
let N41 = NOT N38.
let N37 = (((NOT N30) AND (NOT N4)) OR ((NOT N36) AND (NOT N33))) .
let N39 = NOT N37.
let N40 = (NOT ((N39 AND (NOT N38)) OR ((NOT N39) AND N38))) .
let N12 = NOT N40.
let N35 = (NOT ((N34 AND N30) OR ((NOT N34) AND (NOT N30)))) .
let N14 = NOT N35.
let N44 = (((NOT N41) AND (NOT N37)) OR ((NOT N43) AND (NOT N38))) .

let O[1] = (N15) .
let O[2] = (N13) .
let O[3] = (N14) .
let O[4] = (N12) .
let COUT = (N44) .

let COUT1 = ((CARRYIN AND B[1]) OR (CARRYIN AND A[1]) OR (B[1] AND A[1])).
let COUT2 = ((COUT1 AND B[2]) OR (COUT1 AND A[2]) OR (B[2] AND A[2])).
let COUT3 = ((COUT2 AND B[3]) OR (COUT2 AND A[3]) OR (B[3] AND A[3])).
let ACOUT = ((COUT3 AND B[4]) OR (COUT3 AND A[4]) OR (B[4] AND A[4])).

let AO[1] = ((A[1] XOR B[1]) XOR CARRYIN).
let AO[2] = ((A[2] XOR B[2]) XOR COUT1).
let AO[3] = ((A[3] XOR B[3]) XOR COUT2).
let AO[4] = ((A[4] XOR B[4]) XOR COUT3).

(O[1] == AO[1])
(O[2] == AO[2])
(O[3] == AO[3])
(O[4] == AO[4])
(COUT == ACOUT)
.
