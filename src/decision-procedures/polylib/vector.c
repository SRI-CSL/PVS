/* vector.c
     COPYRIGHT
          Both this software and its documentation are

              Copyrighted 1993 by IRISA /Universite de Rennes I - France,
              Copyright 1995,1996 by BYU, Provo, Utah
                         all rights reserved.

          Permission is granted to copy, use, and distribute
          for any commercial or noncommercial purpose under the terms
          of the GNU General Public license, version 2, June 1991
          (see file : LICENSING).
*/
#include <stdio.h>
#include "stdansi.h"
#include "types.h"
#include "vector.h"

int Factorial(n)
     int n;
{
  int fact, i;

  fact=1;
  for (i=1;i<=n;i++)
    fact*=i;
  return fact;
} /* Factorial */


int Binomial(n, p)
     int n, p;
{
  int prod, i;
  
  if (n-p<p) p=n-p;
  if (p!=0) {
    prod=n-p+1;
    for (i=n-p+2;i<=n;i++)
      prod*=i;
    return prod/Factorial(p);
  } else return 1;
} /* Binomial */

/*----------------------------------------------------------------------*/
/* CNP                                                                  */
/*        a!/(b!(a-b)!) = number of ways to choose b items from a items */
/*----------------------------------------------------------------------*/
int CNP( a, b )
int a,b;
{
        int i;
        double c;
 
        if( a <= b )
                return 1;
        c = 1;
        for( i=a ; i>b ; --i )
                c *= i;
        for( i=1 ; i<=(a-b) ; ++i )
                c /= i;
        return c;
}

int Gcd(a, b)
     int a, b;
{
  int aux;
  
  while (a) { aux=b%a; b=a; a=aux; }
  return (b<0 ? -b : b);
} /* Gcd */


#define Vector_Copy(p1, p2, length) \
  memcpy((char *)p2, (char *)p1, (int)((length)*sizeof(int)))


#define Vector_Init(p1, length) \
  memset((char *)(p1), 0, (int)((length)*sizeof(int)))


Vector *Vector_Alloc(length)
     unsigned length;
{
  Vector *vec;
  vec=(Vector *)malloc(sizeof(vec));
  vec->Size=length;
  vec->p=(int *)malloc(length * sizeof(int));
  return vec;
} /* Vector_Alloc */


void Vector_Free(vec)
     Vector *vec;
{
  free((char *)vec->p);
  free((char *)vec);
} /* Vector_Free */


void Vector_Set(p, n, length)
     int *p, n;
     unsigned length;
{
  int *cp, i;
  
  cp=p;
  for (i=0;i<length;i++)
    *cp++=n;
} /* Vector_Set */


void Vector_Add(p1, p2, p3, length)
     int *p1, *p2, *p3;
     unsigned length;
{
  int *cp1, *cp2, *cp3, i;

  cp1=p1;
  cp2=p2;
  cp3=p3;
  for (i=0;i<length;i++)
    *cp3++=*cp1++ + *cp2++;
} /* Vector_Add */


void Vector_Sub(p1, p2, p3, length)
     int *p1, *p2, *p3;
     unsigned length;
{
  int *cp1, *cp2, *cp3, i;

  cp1=p1;
  cp2=p2;
  cp3=p3;
  for (i=0;i<length;i++)
    *cp3++= *cp1++ - *cp2++;
} /* Vector_Sub */


void Vector_Or(p1, p2, p3, length)
     int *p1, *p2, *p3;
     unsigned length;
{
  int *cp1, *cp2, *cp3, i;

  cp1=p1;
  cp2=p2;
  cp3=p3;
  for (i=0;i<length;i++)
    *cp3++=*cp1++ | *cp2++;
} /* Vector_Or */


void Vector_Scale(p1, p2, lambda, length)
     int *p1, *p2, lambda;
     unsigned length;
{
  int *cp1, *cp2, i;
  
  cp1=p1;
  cp2=p2;
  for (i=0;i<length;i++) {
    *cp2++=*cp1++ * lambda;
  }
} /* Vector_Scale */


void Vector_AntiScale(p1, p2, lambda, length)
     int *p1, *p2, lambda;
     unsigned length;
{
  int *cp1, *cp2, i;
  
  cp1=p1;
  cp2=p2;
  for (i=0;i<length;i++) {
    *cp2++=*cp1++ / lambda;
  }
} /* Vector_Scale */


int Vector_Min(p, length)
     int *p;
     unsigned length;
{
  int *cp, min, i;

  cp=p;
  min=*cp++;
  for (i=1;i<length;i++) {
    if (min<*cp) min=*cp;
    cp++;
  }
  return min;
} /* Vector_Min */


int Vector_Max(p, length)
     int *p;
     unsigned length;
{
  int *cp, max, i;
  
  cp=p;
  max=*cp++;
  for (i=1;i<length;i++)
    if (max>*cp) max=*cp;
  return max;
} /* Vector_Max */


int Inner_Product(p1, p2, length)
     int *p1, *p2;
     unsigned length;
{
  int i, ip;

  ip = *p1++ * *p2++;
  for (i=1;i<length;i++)
    ip += (*p1++ * *p2++);

  return ip;
} /* Inner_Product */

#ifdef CHECK_OVERFLOW

void Vector_Combine(p1, p2, p3, lambda, mu, length)
     int *p1, *p2, *p3, lambda, mu;
     unsigned length;
{ int *cp1, *cp2, *cp3, t1, t2, t3, t4, t5, i;

  cp1=p1;
  cp2=p2;
  cp3=p3;
  for (i=0;i<length;i++)
  {  t4 = *cp1++;
     t5 = *cp2++;
     t1 = lambda * t4;
     t2 = mu * t5;
     if ( (t4 && (lambda != t1/t4)) ||
          (t5 && (mu     != t2/t5)) )
     {   /* multiplication overflow */
#ifndef NO_MESSAGES
	fprintf(stderr, "? Vector_Combine: multiplication overflow\n");
#endif
	Pol_status = 1;
     }
     t3 = t1 + t2;
     if (t1 != t3 - t2)
     {  /* addition overflow */
#ifndef NO_MESSAGES
        fprintf(stderr, "? Vector_Combine: addition overflow\n");
#endif
        Pol_status = 1;
     }
     *cp3++ = t3;
  }
} /* Vector_Combine */

#else

void Vector_Combine(p1, p2, p3, lambda, mu, length)
     int *p1, *p2, *p3, lambda, mu;
     unsigned length;
{
  int *cp1, *cp2, *cp3, i;

  cp1=p1;
  cp2=p2;
  cp3=p3;
  for (i=0;i<length;i++)
    *cp3++=lambda * *cp1++ + mu * *cp2++;
} /* Vector_Combine */

#endif

int Vector_Gcd(p, q, length)
     int *p, *q;
     unsigned length;
{
  int *cq, *cp, min, Index_Min=0, Not_Zero, i;

  for (cq=q, cp=p, i=0 ; i<length ; i++) *cq++=abs(*cp++);
  do
  { min=TOP;
    for (i=0, cq=q ; i<length ; i++, cq++)
      if ( *cq && ((*cq)<min) ) { min=*cq; Index_Min=i; }
    if (min==TOP) min=1;
    if (min!=1)
    { Not_Zero=0;
      for (i=0, cq=q ; i<length; i++, cq++)
        if (i!=Index_Min) Not_Zero|=(*cq%=min);
    } else break;
  } while (Not_Zero);
  return min;
} /* Vector_Gcd */

/* gcd reduce a vector, making sure its pos-th element is positive */
void Vector_Normalize_Positive(p, tmp, length, pos)
     int *p, *tmp;
     int pos, length;
{
  int g, i;

  g = Vector_Gcd( p, tmp, length);	/* g is always positive */
  if ( p[pos]<0 ) g*=-1;           /* make pos-th element positive */
  if ( g!=1 ) for (i=0; i<length; i++) p[i]/=g;		/* normalize */
} /* Vector_Normalize_Positive */

/* gcd reduce a vector */
/* making the last element positive is *not* OK for equalities */
void Vector_Normalize(p, tmp, length)
     int *p, *tmp;
     int length;
{
  int g, i;

  g = Vector_Gcd( p, tmp, length);	/* g is always positive */
  if ( g!=1 ) for (i=0; i<length; i++) p[i]/=g;		/* normalize */
} /* Vector_Normalize */

void Vector_Map(p1, p2, p3, length, f)
     int *p1, *p2, *p3;
     unsigned length;
     int (*f)();
{
  int *cp1, *cp2, *cp3, i;
  
  cp1=p1;
  cp2=p2;
  cp3=p3;
  for (i=0;i<length;i++)
    *cp3=(*f)(*cp1++, *cp2++);
} /* Vector_Map */


int Vector_Reduce(p, length, f)
     int *p;
     unsigned length;
     int (*f)();
{
  int *cp, i, r;
  
  cp=p;
  r=*cp++;
  for(i=1;i<length;i++)
    r=(*f)(r, *cp++);
  return r;
} /* Vector_Insert */


Matrix *Matrix_Alloc(NbRows, NbColumns)
     unsigned NbRows, NbColumns;
{
  Matrix *Mat;
  int **q, *p, i;

  Mat=(Matrix *)malloc(sizeof(Matrix));
  Mat->NbRows=NbRows;
  Mat->NbColumns=NbColumns;
  Mat->p=q=(int **)malloc(NbRows * sizeof(int *));
  Mat->p_Init=p=(int *)malloc(NbRows * NbColumns * sizeof(int));
  for (i=0;i<NbRows;i++) {
    *q++=p;
    p+=NbColumns;
  }
  return Mat;
} /* Matrix_Alloc */


void Matrix_Free(Mat)
     Matrix *Mat;
{
  free((char *) Mat->p_Init);
  free((char *) Mat->p);
  free((char *) Mat);
} /* Matrix_Free */


void Matrix_Vector_Product(mat, p1, p2)
     Matrix *mat;
     int *p1, *p2;
{
  int **cm, *q, *cp1, *cp2, NbRows, NbColumns, i, j;

  NbRows=mat->NbRows;
  NbColumns=mat->NbColumns;
  
  cm=mat->p;
  cp2=p2;
  for (i=0;i<NbRows;i++) {
    q=*cm++;
    cp1=p1;
    *cp2=*q++ * *cp1++;
    for (j=1;j<NbColumns;j++)
      *cp2+= *q++ * *cp1++;
    cp2++;
  }
} /* Matrix_Vector_Product */


void Vector_Matrix_Product(p1, mat, p2)
     Matrix *mat;
     int *p1, *p2;
{
  int **cm, *cp1, *cp2, NbRows, NbColumns, i, j;
  
  NbRows=mat->NbRows;
  NbColumns=mat->NbColumns;
  cp2=p2;
  cm=mat->p;
  for (j=0;j<NbColumns;j++) {
    cp1=p1;
    *cp2=*(*cm+j) * *cp1++;
    for (i=1;i<NbRows;i++)
      *cp2+=*(*(cm+i)+j) * *cp1++;
    cp2++;
  }
} /* Vector_Matrix_Product */


void Matrix_Product(mat1, mat2, mat3)
     Matrix *mat1, *mat2, *mat3;
{
  int **q1, **q2, *p1, *p3, Size, sum, i, j, k;
  unsigned NbRows, NbColumns;

  NbRows    = mat1->NbRows;
  NbColumns = mat2->NbColumns;
  /* mat3 = Matrix_Alloc(NbRows, NbColumns); */
  Size      = mat1->NbColumns;
  if (mat2->NbRows!=Size || mat3->NbRows!=NbRows || mat3->NbColumns!=NbColumns)
  {  fprintf(stderr, "? Matrix_Product : incompatable matrix dimension\n");
     return;
  }
  
  p3=mat3->p_Init;
  q1=mat1->p;
  q2=mat2->p;
  for (i=0;i<NbRows;i++) {
    for (j=0;j<NbColumns;j++) {
      p1=*(q1+i);
      sum=0;
      for (k=0;k<Size;k++)
        sum+=*p1++ * *(*(q2+k)+j);
      *p3++=sum;
    }
  }
} /* Vector_Matrix_Product */


void Vector_Print(Dst, Format, Vec)
     FILE *Dst;
     char *Format;
     Vector *Vec;
{
  int i, *p;
  unsigned length;
  
  (void) fprintf(Dst, "%d\n", length=Vec->Size);
  p=Vec->p;
  for (i=0;i<length;i++)
    (void) fprintf(Dst, Format, *p++);
  (void) fprintf(Dst, "\n");
} /* Vector_Print */


Vector *Vector_Read()
{
  Vector *Vec;
  unsigned length;
  int *p, i;

  (void) scanf("%d", &length);
  Vec = Vector_Alloc(length);
  p=Vec->p;
  for (i=0;i<length;i++)
    (void) scanf("%d", p++);
  return Vec;
} /* Vector_Read */


void Matrix_Print(Dst, Format, Mat)
     FILE* Dst;
     char *Format;
     Matrix *Mat;
{
  int *p, i, j;
  unsigned NbRows, NbColumns;

  fprintf(Dst,"%d %d\n", NbRows=Mat->NbRows, NbColumns=Mat->NbColumns);
  for (i=0;i<NbRows;i++) {
    p=*(Mat->p+i);
    for (j=0;j<NbColumns;j++)
      fprintf(Dst, Format, *p++);
      fprintf(Dst, "\n");
  }
} /* Matrix_Print */

/* a '#' in the first column is a comment line */
Matrix *Matrix_Read()
{
  Matrix *Mat;
  int *p, i, j, n;
  unsigned NbRows, NbColumns;
  char *c, s[128];

  fgets(s, 128, stdin);
  while ( (*s=='#') || (sscanf(s, " %d %d", &NbRows, &NbColumns)<2) )
    fgets(s, 128, stdin);
  Mat = Matrix_Alloc(NbRows, NbColumns);
  p = Mat->p_Init;
  for (i=0;i<NbRows;i++)
  { c = fgets(s, 128, stdin);
    while(c && *c=='#') c = fgets(s, 128, stdin);
    if (!c)
    {  fprintf(stderr, "Matrix_Read: ?not enough rows\n");
       break;
    }
    for (j=0;j<NbColumns;j++)
    { if (sscanf(c, " %d%n", p++, &n)==0)
      {  fprintf(stderr, "Matrix_Read: ?not enough columns\n");
         break;
      }
      c += n;
    }
  }
  return Mat;
} /* Matrix_Read */


int First_Non_Zero(p, length)
     int *p;
     unsigned length;
{
  int *cp, i;
  
  cp=p;
  for (i=0;i<length;i++)
    if (*cp++) break;
  return (i==length) ? -1 : i;
} /* First_Non_Null */

void Vector_Sort(Vec, n)
     int *Vec;
     unsigned n;	/* length */
{
  int i, j, temp;
  int *current_node=(int *)0,
      *left_son,
      *right_son;

  for (i=(n-1)/2;i>=0;i--) {         /* phase 1 : build the heap */
    j=i;
    temp=*(Vec+i);
    while (j<=(n-1)/2) {             /* while not a leaf         */
      current_node=Vec+j;
      left_son=Vec+(j<<1)+1;
      if ((j<<1)+2>=n) {             /* only one son             */
        if (temp<*left_son) {
          *current_node=*left_son;
          j=(j<<1)+1;
        } else break;
      } else {                       /* two sons                 */
        right_son=left_son+1;
        if (*right_son<*left_son) {
          if (temp<*left_son) {
            *current_node=*left_son;
            j=(j<<1)+1;
          } else break;
        } else {
          if (temp<*right_son) {
            *current_node=*right_son;
            j=(j<<1)+2;
          } else break;
        }
      }
    }
    *current_node=temp;
  }
  for (i=n-1;i>0;i--) {              /* phase 2 : sort the heap */
    temp=*(Vec+i);
    *(Vec+i)=*Vec;
    j=0;
    while (j<i/2) {                  /* while not a leaf        */
      current_node=Vec+j;
      left_son=Vec+(j<<1)+1;
      if ((j<<1)+2>=i) {             /* only one son            */
        if (temp<*left_son) {
          *current_node=*left_son;
          j=(j<<1)+1;
        } else break;
      } else {                       /* two sons                */
        right_son=left_son+1;
        if (*right_son<*left_son) {
          if (temp<*left_son) {
            *current_node=*left_son;
            j=(j<<1)+1;
          } else break;
        } else {
          if (temp<*right_son) {
            *current_node=*right_son;
            j=(j<<1)+2;
          } else break;
        }
      }
    }
    *current_node=temp;
  }
} /* Vector_Sort */


int Vector_Equal(Vec1, Vec2, n)
     int *Vec1, *Vec2;
     unsigned n;
{
  int i, *p1, *p2;

  p1=Vec1;
  p2=Vec2;
  for (i=0;i<n;i++)
    if (*p1++!=*p2++) break;
  
  return (i==n);
} /* Vector_Equal */
