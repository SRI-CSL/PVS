/*
 DOCUMENTATION INFORMATION				        module: DOUBLE
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : HP 9000/S700
 file	   : double.c
 unit-title: 
 ref.	   : Suggested by Andreas Kuehlmann, IBM Yorktown
 author(s) : Copyright (c) 1993-1998 G.L.J.M. Janssen
 date	   :  8-JAN-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ------------------------------------------------------------------------ */
/* INCLUDES                                                                 */
/* ------------------------------------------------------------------------ */

#include <stdio.h>
#include <limits.h>

#include "double.h"

/* ------------------------------------------------------------------------ */
/* LOCAL DEFINES                                                            */
/* ------------------------------------------------------------------------ */

/* An all 1's Word value: */
#define MaxWord		(~((Word) 0))

/* Maximum exponent value: */
#define MaxExp		(((unsigned int) 1 << D_NR_EXP_BITS) - 1)

/* Mask for the least-significant i (>= 0) bits of a Word: */
#define MASKS(i)	(~(MaxWord << (i)))

/* Watch out for shifts with second operand a variable/expression with a value
   of 32 bits or more! Some machines don't correctly calculate the required
   0 result. All shifts in this source file are okay, otherwise could
   use the following macros.
*/
#define SHR(w,i)	((i) >= 32 ? ((Word) 0) : (w) >> (i))
#define SHL(w,i)	((i) >= 32 ? ((Word) 0) : (w) << (i))

#define MSB_MASK	LONG_MIN

/* ------------------------------------------------------------------------ */
/* LOCAL TYPE DEFINITIONS                                                   */
/* ------------------------------------------------------------------------ */

/*typedef unsigned long Word;*/
/* On AIX: */
typedef unsigned int Word;

/* ------------------------------------------------------------------------ */
/* LOCAL VARIABLES                                                          */
/* ------------------------------------------------------------------------ */
static char SccsId[] = "%Z%%Y%/%M% %I% %G%";

/* ------------------------------------------------------------------------ */
/* EXPORTED VARIABLES                                                       */
/* ------------------------------------------------------------------------ */

/* Some normalized constant values of type Double. */
const Double Double_0   = {0,0,0,0};
const Double Double_1   = {0,0,0,1};
const Double Double_inf = {0,
			   (unsigned int) 0xFFFF,
			   (unsigned int) 0x7FFF,
			   (unsigned int) 0xFFFFFFFF};

/* ------------------------------------------------------------------------ */
/* LOCAL FUNCTION PROTOTYPES                                                */
/* ------------------------------------------------------------------------ */

/* ************************************************************************ */
/* FUNCTION DOCUMENTATION:                                                  */
/*                                                                          */
/* ************************************************************************ */

/* To assign Doubles simply use C = operator. */

/* Returns 1 if d is exact, else 0.
   A Double number may become inexact because of rounding errors during
   the operations.
*/
int D_exact (Double d)
{
  return !D_INEXACT (d);
}

/* Shifts mantissa over `delta_exp' bits to the right (towards LSB).
   If any of the lower `delta_exp' bits is 1 then result will loose
   those bits; this is indicated by setting the inexact flag.
   Exponent is incremented with `delta_exp' to correct the shift.
   Result is typically not normalized!
*/
static void D_align_exp (Double *d, unsigned long delta_exp)
{
  Word diff;
  Word over;

  if (!delta_exp)
    /* Zero shift: no effect. */
    return;

  if (delta_exp >= D_NR_MANTISSA_BITS) {
    /* Result will be unnormalized 0. Perhaps still exact? */
    if (D_H_MANTISSA (*d) || D_L_MANTISSA (*d)) {
      /* 1 bits in mantissa are present; result will be inexact. */
      D_L_MANTISSA (*d) = 0;
      D_H_MANTISSA (*d) = 0;
      D_INEXACT (*d) = 1;
    }
    /* else D_L_MANTISSA (*d) == 0 && D_H_MANTISSA (*d) == 0 */

    /* Must still adjust exponent. */
  }
  else
  /* Here: 0 < delta_exp < D_NR_MANTISSA_BITS */
  if (delta_exp >= D_NR_L_MANTISSA_BITS) {
    /* D_NR_L_MANTISSA_BITS
       <= delta_exp < D_NR_L_MANTISSA_BITS+D_NR_H_MANTISSA_BITS */

    diff = delta_exp - D_NR_L_MANTISSA_BITS;
    /* 0 <= diff < D_NR_H_MANTISSA_BITS */

    /* Test for exactness (all bits that shift out must be 0): */
    if ((D_H_MANTISSA (*d) & MASKS (diff)) || D_L_MANTISSA (*d))
      /* Some shifted-out bits are 1. */
      D_INEXACT (*d) = 1;

    /* Right shift higher bits of mantissa: */
    D_L_MANTISSA (*d) = (D_H_MANTISSA (*d) >> diff);
    D_H_MANTISSA (*d) = 0;
  }
  else {
    /* 0 < delta_exp < D_NR_L_MANTISSA_BITS */

    /* Test for exactness: */
    if (D_L_MANTISSA (*d) & MASKS (delta_exp))
      /* Some shifted-out bits are 1. */
      D_INEXACT (*d) = 1;

    /* Right shift lower bits of mantissa: */
    D_L_MANTISSA (*d) >>= delta_exp;

    /* Get bits that move over from high part to lower part of mantissa: */
    if (delta_exp >= D_NR_H_MANTISSA_BITS) {
      /* All bits move over: */
      over = D_H_MANTISSA (*d);
      D_H_MANTISSA (*d) = 0;
    }
    else {
      /* Less than D_NR_H_MANTISSA_BITS bits move over: */
      over = D_H_MANTISSA (*d) & MASKS (delta_exp);
      /* Right shift higher bits of mantissa: */
      D_H_MANTISSA (*d) >>= delta_exp;
    }

    /* Align `over' to fit precisely to the left new lower part: */
    over <<= (D_NR_L_MANTISSA_BITS - delta_exp);

    /* Include the over bits in lower mantissa: */
    D_L_MANTISSA (*d) |= over;
  }

  /* Adjust exponent (guard against overflow): */
  if ((delta_exp += D_EXP (*d)) <= MaxExp)
    D_EXP (*d) = delta_exp;
  else {
    D_EXP (*d) = MaxExp;
    D_INEXACT (*d) = 1;
  }
  /* Note: result *d might be 0 * 2^something.
     This is correct and necessary because we want the exponent to reflect
     the shift over `delta_exp' no matter what the mantissa value is.
  */
}

/* Aligns the number arguments to have equal exponents.
   Might cause the one with smallest exponent to become inexact
   and/or not-normalized. Arguments are assumed normalized.
*/
static void D_align (Double *d1, Double *d2)
{
  Word d1_exp = D_EXP (*d1);
  Word d2_exp = D_EXP (*d2);

  if (d1_exp < d2_exp)
    D_align_exp (d1, d2_exp - d1_exp);
  else
  if (d1_exp > d2_exp)
    D_align_exp (d2, d1_exp - d2_exp);
  /* else: exponents are equal. */
}

/* Normalizes the number d, i.e., makes sure d has minimal exponent value.
   Involves left-shift of mantissa, i.e. towards MSB.
   Of course won't cause any loss in precision.
*/
static Double *D_normalize (Double *d)
{
  Word h_mantissa = D_H_MANTISSA (*d);
  Word l_mantissa = D_L_MANTISSA (*d);

  if (l_mantissa || h_mantissa) {
    Word H_MSB = 1 << (D_NR_H_MANTISSA_BITS - 1);

    /* Shift left as long as MSB of higher mantissa is not set: */
    while (D_EXP (*d) && !(h_mantissa & H_MSB)) {
      /* Left-shift higher mantissa over 1 bit position: */
      h_mantissa <<= 1;
      /* Include the MSB of lower mantissa as LSB of higher: */
      h_mantissa |= !!(l_mantissa &
		       ((unsigned int) 1 << (D_NR_L_MANTISSA_BITS - 1)));
      /* Left-shift lower mantissa over 1 bit position: */
      l_mantissa <<= 1;
      D_EXP (*d)--;
    }
    D_H_MANTISSA (*d) = h_mantissa;
    D_L_MANTISSA (*d) = l_mantissa;
  }
  else
    /* mantissa == 0; make its exponent 0 as well: */
    D_EXP (*d) = 0;
  return d;
}

/* Makes d > 0 have maximal exponent value without loss in precision.
   Involves right-shift of mantissa, i.e. towards LSB.
*/
static Double *D_max_exp (Double *d)
{
  Word h_mantissa = D_H_MANTISSA (*d);
  Word l_mantissa = D_L_MANTISSA (*d);

  if (l_mantissa || h_mantissa) {
    /* Shift right as long as LSB of lower mantissa is not set: */
    while (D_EXP (*d) < MaxExp && !(l_mantissa & 1)) {
      /* Right-shift lower mantissa over 1 bit position: */
      l_mantissa >>= 1;
      /* Include the LSB of higher mantissa as MSB of lower: */
      l_mantissa |= (h_mantissa & 1) << (D_NR_L_MANTISSA_BITS - 1);
      /* Right-shift higher mantissa over 1 bit position: */
      h_mantissa >>= 1;
      D_EXP (*d)++;
    }
    D_H_MANTISSA (*d) = h_mantissa;
    D_L_MANTISSA (*d) = l_mantissa;
  }
  else
    /* mantissa == 0; make its exponent 0 as well: */
    D_EXP (*d) = 0;
  return d;
}

/* Returns d1 + d2. */
Double D_add (Double d1, Double d2)
{
  Double d;
  Word carry, H;
  Word d1_MSB, d2_MSB, s_MSB;
  Word l_mantissa;
  Word h_mantissa;

  D_align (&d1, &d2);
  /* Here: d1 and d2 exponents are equal. */

  /* Add mantissas: */

  /* Add lower parts with MSB cleared: */

  /* Get d1 MS bit: */
  d1_MSB = !!(D_L_MANTISSA (d1) & MSB_MASK);
  /* Clear that bit: */
  D_L_MANTISSA (d1) &= ~MSB_MASK;

  /* Get d2 MS bit: */
  d2_MSB = !!(D_L_MANTISSA (d2) & MSB_MASK);
  /* Clear that bit: */
  D_L_MANTISSA (d2) &= ~MSB_MASK;

  /* Regular unsigned addition guaranteed without overflow: */
  l_mantissa = D_L_MANTISSA (d1) + D_L_MANTISSA (d2);

  /* Get result MS bit: */
  s_MSB = !!(l_mantissa & MSB_MASK);

  /* Handle MS bit: */
  H = d1_MSB + d2_MSB + s_MSB;
  if (H & 1)
    l_mantissa |=  MSB_MASK;
  else
    l_mantissa &= ~MSB_MASK;
  /* Carry bit of lower mantissas addition: */
  carry = H >> 1;

  /* Add higher parts with carry: */

  h_mantissa = D_H_MANTISSA (d1) + D_H_MANTISSA (d2) + carry;

  /* Get overflow info: */
  carry = h_mantissa & (1 << D_NR_H_MANTISSA_BITS);
  /* Clear the carry bit: */
  h_mantissa &= ~(1 << D_NR_H_MANTISSA_BITS);

  D_H_MANTISSA (d) = h_mantissa;
  D_L_MANTISSA (d) = l_mantissa;
  D_EXP        (d) = D_EXP (d1);
  D_INEXACT    (d) = D_INEXACT (d1) || D_INEXACT (d2);

  if (carry) {
    D_align_exp (&d, 1);
    /* Include the carry bit as MSB of higher mantissa: */
    D_H_MANTISSA (d) |= (carry >> 1);
  }
  /* Result is in normalized form. */
  return d;
}

/* Returns if d1 > d2 then d1 - d2 else 0. (Monus operation) */
Double D_sub (Double d1, Double d2)
{
  Double d;
  Word borrow, H;
  Word d1_MSB, d2_MSB, s_MSB;
  Word l_mantissa;
  Word h_mantissa;

  D_align (&d1, &d2);
  /* Here: d1 and d2 exponents are equal. */

  /* Subtract mantissas: */

  /* Subtract lower parts with MSB cleared: */

  /* Get d1 MS bit: */
  d1_MSB = !!(D_L_MANTISSA (d1) & MSB_MASK);
  /* Clear that bit: */
  D_L_MANTISSA (d1) &= ~MSB_MASK;

  /* Get d2 MS bit: */
  d2_MSB = !!(D_L_MANTISSA (d2) & MSB_MASK);
  /* Clear that bit: */
  D_L_MANTISSA (d2) &= ~MSB_MASK;

  /* Regular unsigned subtraction guaranteed without underflow: */
  l_mantissa = D_L_MANTISSA (d1) - D_L_MANTISSA (d2);

  /* Get result MS bit: */
  s_MSB = !!(l_mantissa & MSB_MASK);

  /* Handle MS bit: */
  H = d1_MSB + d2_MSB + s_MSB;
  if (H & 1)
    l_mantissa |=  MSB_MASK;
  else
    l_mantissa &= ~MSB_MASK;
  /* Borrow bit of lower mantissas subtraction: */
  borrow = (!d1_MSB + d2_MSB + s_MSB) >> 1;

  /* Subtract higher parts with borrow: */

  h_mantissa = D_H_MANTISSA (d1) - D_H_MANTISSA (d2) - borrow;

  /* Get underflow info: */
  borrow = h_mantissa & (1 << D_NR_H_MANTISSA_BITS);

  if (borrow) {
    d = Double_0;
    D_INEXACT (d) = 1;
    return d;
  }

  D_H_MANTISSA (d) = h_mantissa;
  D_L_MANTISSA (d) = l_mantissa;
  D_EXP        (d) = D_EXP (d1);
  D_INEXACT    (d) = D_INEXACT (d1) || D_INEXACT (d2);

  /* Result is in normalized form. */
  return *D_normalize (&d);
}

/* Returns d * 2^exp; exp >= 0.
   Assumes d is normalized upon entry.
*/
Double D_times2up (Double d, unsigned long exp)
{
  /* Adjust exponent (guard against overflow): */
  if ((exp += D_EXP (d)) <= MaxExp)
    D_EXP (d) = exp;
  else {
    D_EXP (d) = MaxExp;
    D_INEXACT (d) = 1;
  }
  /* Result is in normalized form. */
  return *D_normalize (&d);
}

/* Returns d / 2^exp; exp >= 0.
   Assumes d is normalized upon entry.
*/
Double D_divide2up (Double d, unsigned long exp)
{
  if (exp <= D_EXP (d))
    D_EXP (d) -= exp;
  else {
    /* exp > D_EXP (d) */

    D_align_exp (&d, exp - D_EXP (d));
    D_EXP (d) = 0;
  }
  /* Result is in normalized form. */
  return d;
}

/* Returns 2^exp; exp >= 0. */
Double D_2up (unsigned long exp)
{
  /* Result is in normalized form. */
  return D_times2up (Double_1, exp);
}

/* Returns:
   -1 if d1 < d2,
    0 if d1 = d2,
    1 if d1 > d2
*/
long D_compare (Double d1, Double d2)
{
  /* Assumes d1 and d2 are normalized. */

  if (D_EXP (d1) < D_EXP (d2))
    return -1;

  if (D_EXP (d1) > D_EXP (d2))
    return 1;

  /* Here: d1 and d2 exponents are equal. */

  if (D_H_MANTISSA (d1) < D_H_MANTISSA (d2))
    return -1;

  if (D_H_MANTISSA (d1) > D_H_MANTISSA (d2))
    return 1;

  /* Here: D_H_MANTISSA (d1) == D_H_MANTISSA (d2) */

  if (D_L_MANTISSA (d1) < D_L_MANTISSA (d2))
    return -1;

  if (D_L_MANTISSA (d1) > D_L_MANTISSA (d2))
    return 1;

  return 0;
}

/* Constructs a Double from a C unsigned long. */
Double D_cast (unsigned long n)
{
  Double d;

  d = Double_0;

  D_L_MANTISSA (d) = n;
  /* d is in normalized form. */
  return d;
}

void D_convert2C (Double d, double *mantissa, unsigned long *exp)
{
  /* Using a trick here to convert mantissa to decimal:
     hoping that in calculating the double no rounding errors
     occur and/or precision is lost.
     This is guaranteed when 2^D_NR_MANTISSA_BITS < DMAXPOWTWO
     (see values.h).
     Most double formats have more than our 47 bits significant,
     so our assumption is justified.
     (On a HP9000s700 the maximum power of 2 exactly representable
     by a double is 2^52).
  */
  *mantissa = (  (double) D_H_MANTISSA (d)
	       /* Watch out for sign bit! */
	       * (double) (1 << (D_NR_L_MANTISSA_BITS - 2)) /* = 2^30 */
	       * 4.0)
            + (double) D_L_MANTISSA (d);

  *exp = D_EXP (d);
}

char *D_sprintf (char *buf, Double d, int maximize_exp)
{
  int mantissa1;
  char *p = buf;

  p[0] = '\0';

  if (maximize_exp) D_max_exp (&d);

  if (D_H_MANTISSA (d)) {
    double g;
    unsigned long exp;

    D_convert2C (d, &g, &exp);
/*      fprintf (stdout, "d: inexact = %u, exp = %u, h_mantissa = %u, l_mantissa = %u\n", D_INEXACT(d), D_EXP(d), D_H_MANTISSA(d), D_L_MANTISSA(d)); */
/*      fprintf (stdout, "D_convert: g = %u, exp = %u\n", d, g, exp); */

    /* Note: on some UNIX sprintf does not return #chars transferred!
       Then use:  sprintf (p, "%.0f", g); while (*p) p++, etc.
    */
    /* p += sprintf (p, "%.0f", g); */
    sprintf (p, "%.0f", g); while (*p) p++;

    mantissa1 = 0;
  }
  else
    /* Special action for mantissa == 1:
       don't output 1*, just the 2-power.
    */
    if (!(mantissa1 = D_L_MANTISSA (d) == 1) || !D_EXP (d))
      /* p += sprintf (p, "%u", D_L_MANTISSA (d)); */
      sprintf (p, "%u", D_L_MANTISSA (d)); while (*p) p++;

  if (D_EXP (d))
    /* p += sprintf (p, "%s2^%u", mantissa1 ? "" : "*", D_EXP (d)); */
    sprintf (p, "%s2^%u", mantissa1 ? "" : "*", D_EXP (d)); while (*p) p++;

/*
  p += sprintf (p, " Hex: ");
  p += sprintf (p, "0x%0X", D_H_MANTISSA (d));
  p += sprintf (p, "%0X", D_L_MANTISSA (d));
*/

  /* p += sprintf (p, "%s", D_INEXACT (d) ? " (approx)" : ""); */
  sprintf (p, "%s", D_INEXACT (d) ? " (approx)" : ""); while (*p) p++;

  return buf;
}

void D_fprintf (FILE *fp, Double d)
{
  char buf[D_BUFSIZ];

  fputs (D_sprintf (buf, d, 0), fp);
}

void D_printf (Double d)
{
  char buf[D_BUFSIZ];

  fputs (D_sprintf (buf, d, 0), stdout);
}
