to_be or not to_be . /* Shakespeare */

not (a and b) equiv not a or not b . /* De Morgan */

(x and not x) . /* contradiction */

x equiv y :- y implies x, y equiv a or not a .

(y implies x) and (y equiv a or not a) -> (x equiv y) .

not E :- ((A and W) implies P), /* Superman */
         (not A implies I),
         (not W implies M),
         (not P),
         (E implies not I and not M) .

/* Portrait in the casket */
G :-
        /* Gold: I'm the one: */
        (g equiv G),
        /* Silver: I'm the one: */
        (s equiv S),
        /* Lead: >= 2 lie */
        (l equiv ((not g and not s)     /* gold and silver lie */
                  or
                  (not g and not l)     /* gold and lead lie */
                  or
                  (not s and not l)     /* silver and lead lie */
/*                 or */
/*                 (not g and not s and not l)   /* all lie */
                  )),
        /* the portrait is somewhere: */
        ((G and not S and not L)        /* in gold box */
         or
         (not G and S and not L)        /* in silver box */
         or
         (not G and not S and L))       /* in leaden box */
         .

S :-
        /* Gold: I'm the one: */
        (g equiv G),
        /* Silver: I'm the one: */
        (s equiv S),
        /* Lead: >= 2 lie */
        (l equiv ((not g and not s)     /* gold and silver lie */
                  or
                  (not g and not l)     /* gold and lead lie */
                  or
                  (not s and not l)     /* silver and lead lie */
/*                 or */
/*                 (not g and not s and not l)   /* all lie */
                  )),
        /* the portrait is somewhere: */
        ((G and not S and not L)        /* in gold box */
         or
         (not G and S and not L)        /* in silver box */
         or
         (not G and not S and L))       /* in leaden box */
         .

L :-
        /* Gold: I'm the one: */
        (g equiv G),
        /* Silver: I'm the one: */
        (s equiv S),
        /* Lead: >= 2 lie */
        (l equiv ((not g and not s)     /* gold and silver lie */
                  or
                  (not g and not l)     /* gold and lead lie */
                  or
                  (not s and not l)     /* silver and lead lie */
/*                 or */
/*                 (not g and not s and not l)   /* all lie */
                  )),
        /* the portrait is somewhere: */
        ((G and not S and not L)        /* in gold box */
         or
         (not G and S and not L)        /* in silver box */
         or
         (not G and not S and L))       /* in leaden box */
         .

/* The AND truth-table: */

x and y :- not x, not y .
x and y :- not x,     y .
x and y :-     x, not y .
x and y :-     x,     y .

/* The IMPLIES truth-table: */

x implies y :- not x, not y .
x implies y :- not x,     y .
x implies y :-     x, not y .
x implies y :-     x,     y .
