/*
 DOCUMENTATION INFORMATION				        module: DOUBLE
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : HP 9000/S700
 file	   : double.h
 unit-title: 
 ref.	   : Suggested by Andreas Kuehlmann, IBM Yorktown
 author(s) : Copyright (c) 1993-1998 G.L.J.M. Janssen
 date	   :  8-JAN-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifndef DOUBLE_H
#define DOUBLE_H

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <stdio.h>

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* The D_NR_EXP_BITS constant is arbitrary, all the other defines depend on it;
   Limits are: 1 <= D_NR_EXP_BITS <= BITS (Word) - 2
   D_NR_EXP_BITS is the number of bits of the exponent.
*/
#define D_NR_EXP_BITS		16

/* Number of bits of an `unsigned int': */
#define	D_NR_WORD_BITS		32
#define	D_NR_H_MANTISSA_BITS	(D_NR_WORD_BITS-D_NR_EXP_BITS-1)
#define	D_NR_L_MANTISSA_BITS	D_NR_WORD_BITS
#define	D_NR_MANTISSA_BITS	(D_NR_H_MANTISSA_BITS+D_NR_L_MANTISSA_BITS)

/* Access to fields of Double struct: */
#define D_EXP(D)		((D).exp)
#define D_L_MANTISSA(D) 	((D).l_mantissa)
#define D_H_MANTISSA(D)		((D).h_mantissa)
#define D_INEXACT(D)		((D).inexact)

/* Safe value for buffer that holds Double number in decimal format.
   128 is more than we will ever need because largest string is
   "140737488355327*2^65535 (approx)"
    12345678901234567890123456789012
*/
#define D_BUFSIZ		128

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* This struct is supposed to have sizeof (Double) = 8 */
typedef struct {
  unsigned int inexact    : 1;
  unsigned int exp        : D_NR_EXP_BITS;
  unsigned int h_mantissa : D_NR_H_MANTISSA_BITS;
  unsigned int l_mantissa : D_NR_L_MANTISSA_BITS;
} Double;

/*
   Layout of Double record:

          31 | 30 29 28 27 ...  15 | 14 13 ... 6 5 4 3 2 1 0
         +---+---------------------+------------------------+
         | I |         exp         |      h_mantissa        |
         +---+---------------------+------------------------+
         |                   l_mantissa                     |
         +--------------------------------------------------+
*/

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */
static char SccsId_DOUBLE_H[] = "%Z%%Y%/%M% %I% %G%";

/* Some useful Double constants: */
extern const Double Double_0;
extern const Double Double_1;
/* Largest number representable as a Double: */
extern const Double Double_inf;

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern Double D_cast      (unsigned long n);
extern    int D_exact     (Double d);
extern Double D_add       (Double d1, Double d2);
extern Double D_sub       (Double d1, Double d2);
extern Double D_2up       (unsigned long exp);
extern Double D_times2up  (Double d, unsigned long exp);
extern Double D_divide2up (Double d, unsigned long exp);
extern   long D_compare   (Double d1, Double d2);

extern   void D_convert2C (Double d, double *mantissa, unsigned long *exp);
extern  char *D_sprintf	  (char *buf, Double d, int maximize_exp);
extern   void D_fprintf   (FILE *fp, Double d);
extern   void D_printf    (Double d);

#endif /* DOUBLE_H */
