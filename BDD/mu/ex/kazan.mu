/* Hans Kazan's travel trick.
   [cast in MU by (c) Copyright G. Janssen]

   For complete description see ~/ptl/ptl/ex/kazan.ptl.
   NOT FINISHED!

   Rome     Berlin  Amsterdam

   Geneva   London  Brussels

   Hamburg  Paris   Vienna
*/

let S0 = L
 Rome, Berlin, Amsterdam, Geneva, London, Brussels, Hamburg, Paris, Vienna.

/* Pick any of the next 4 cities as starting point: */
one_of(Geneva, Berlin, Brussels, Paris)
&
/* So you may not start in any one of these: */
Rome' & Amsterdam' & London' & Hamburg' & Vienna'
;

/* Possible moves: */
let Moves1 = L

 Rome, Berlin, Amsterdam, Geneva, London, Brussels, Hamburg, Paris, Vienna,
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna.

(@Rome      == Geneva + Berlin)
&
(@Berlin    == Rome + Amsterdam + London)
&
(@Amsterdam == Berlin + Brussels)
&
(@Geneva    == Rome + London + Hamburg)
&
(@London    == Berlin + Geneva + Brussels + Paris)
&
(@Brussels  == London + Amsterdam + Vienna)
&
(@Hamburg   == Geneva + Paris)
&
(@Paris     == Hamburg + London + Vienna)
&
(@Vienna    == Paris + Brussels)
;

/* Do 4 steps either horizontally or vertically (not diagonally): */
let Moves1_2 =
L
 Rome, Berlin, Amsterdam, Geneva, London, Brussels, Hamburg, Paris, Vienna,
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna.
E
c1,c2,c3,c4,c5,c6,c7,c8,c9
.
Moves1 (
 Rome, Berlin, Amsterdam, Geneva, London, Brussels, Hamburg, Paris, Vienna,
c1,c2,c3,c4,c5,c6,c7,c8,c9
)
&
Moves1 (
c1,c2,c3,c4,c5,c6,c7,c8,c9,
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna
)
;

let Moves1_4 =
L
 Rome, Berlin, Amsterdam, Geneva, London, Brussels, Hamburg, Paris, Vienna,
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna.
E
c1,c2,c3,c4,c5,c6,c7,c8,c9
.
Moves1_2 (
 Rome, Berlin, Amsterdam, Geneva, London, Brussels, Hamburg, Paris, Vienna,
c1,c2,c3,c4,c5,c6,c7,c8,c9
)
&
Moves1_2 (
c1,c2,c3,c4,c5,c6,c7,c8,c9,
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna
)
;

E
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna.
S0 (
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna
)
&
Moves1_4 (
@Rome,@Berlin,@Amsterdam,@Geneva,@London,@Brussels,@Hamburg,@Paris,@Vienna,
 Rome, Berlin, Amsterdam, Geneva, London, Brussels, Hamburg, Paris, Vienna
)
;

-----------------

/* From now on exclude Vienna: */
@{4}[] !Vienna

/* Do another 5 steps: */
@{4}[5](
(@Rome      == Geneva + Berlin)
(@Berlin    == Rome + Amsterdam + London)
(@Amsterdam == Berlin + Brussels)
(@Geneva    == Rome + London + Hamburg)
(@London    == Berlin + Geneva + Brussels + Paris)
(@Brussels  == London + Amsterdam)
(@Hamburg   == Geneva + Paris)
(@Paris     == Hamburg + London)
)

/* From now on exclude Brussels: */
@{9}[] !Brussels

/* Do another 2 steps: */
@{9}[2](
(@Rome      == Geneva + Berlin)
(@Berlin    == Rome + Amsterdam + London)
(@Amsterdam == Berlin)
(@Geneva    == Rome + London + Hamburg)
(@London    == Berlin + Geneva + Paris)
(@Hamburg   == Geneva + Paris)
(@Paris     == Hamburg + London)
)

/* From now on exclude Paris: */
@{11}[] !Paris

/* Do another 3 steps: */
@{11}[3](
(@Rome      == Geneva + Berlin)
(@Berlin    == Rome + Amsterdam + London)
(@Amsterdam == Berlin)
(@Geneva    == Rome + London + Hamburg)
(@London    == Berlin + Geneva)
(@Hamburg   == Geneva)
)

/* From now on exclude Hamburg and Amsterdam: */
@{14}[] (!Hamburg !Amsterdam)

/* Do another 3 steps: */
@{14}[3](
(@Rome      == Geneva + Berlin)
(@Berlin    == Rome + London)
(@Geneva    == Rome + London)
(@London    == Berlin + Geneva)
)

/* From now on exclude Berlin: */
@{17}[] !Berlin

/* Do the last single step: */
@{17}[1](
(@Rome      == Geneva)
(@Geneva    == Rome + London)
(@London    == Geneva)
)

/* We must have ended up in Geneva: */
->
@{18} (Geneva !Rome !London)
.
