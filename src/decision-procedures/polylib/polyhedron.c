/* polyhedron.c
     COPYRIGHT
          Both this software and its documentation are

              Copyright 1993 by IRISA /Universite de Rennes I - France,
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
#include "polyhedron.h"

#define exchange(a, b, t)\
{ (t)=(a); (a)=(b); (b)=(t); }

#define bexchange(a, b, t, l)\
{\
  memcpy((char *)(t), (char *)(a), (int)(l));\
  memcpy((char *)(a), (char *)(b), (int)(l));\
  memcpy((char *)(b), (char *)(t), (int)(l));\
}

#define WSIZE (8*sizeof(int))

#define Vector_Copy(p1, p2, length) \
  memcpy((char *)(p2), (char *)(p1), (int)(length)*sizeof(int))

#define Vector_Init(p1, length) \
  memset((char *)(p1), 0, (int)(length)*sizeof(int))

int Pol_status;                    /* error status after operations */

/* Combine --
     (p3) = linear combination of (p1) and (p2)
     such that (p3[pos]) is zero.
     First element of each vector is status word and is not
     changed in (p3).  (pos) may be 0 however.
     (length) does not include status word.1
*/
static void Combine(p1, p2, p3, pos, temp, length)
     int *p1, *p2, *p3, pos;
     int *temp;
     unsigned length;
{ int a, a1, a2;

  a1 = p1[pos]; a2 = p2[pos];
  a  = Gcd(abs(a1), abs(a2));
  a1 = a1/a;
  a2 = a2/a;
  Vector_Combine(p1+1, p2+1, p3+1, a2, -a1, length);
  Vector_Normalize(p3+1, temp, length);
} /* Combine */

Polyhedron *Polyhedron_Alloc(Dimension, NbConstraints, NbRays)
     unsigned Dimension, NbConstraints, NbRays;
{ Polyhedron *Pol;
  unsigned NbRows,NbColumns;
  int i, *p, **q;

  Pol=(Polyhedron *)malloc(sizeof(Polyhedron));
  Pol->next          = (Polyhedron *)0;
  Pol->Dimension     = Dimension;
  Pol->NbConstraints = NbConstraints;
  Pol->NbRays        = NbRays;
  Pol->NbEq          = 0;
  Pol->NbBid         = 0;
  NbRows             = NbConstraints + NbRays;
  NbColumns          = Dimension + 2;
  Pol->Constraint    = q = (int **)malloc(NbRows * sizeof(int *));
  Pol->Ray           = q + NbConstraints;
  Pol->p_Init        = p = (int *)malloc(NbRows * NbColumns * sizeof(int));

  for (i=0;i<NbRows;i++)
  { *q++ = p;
    p += NbColumns;
  }
  return Pol;
} /* Polyhedron_Alloc */

Polyhedron *Polyhedron_Copy (Pol)
    Polyhedron *Pol;
{ Polyhedron *Pol1;
  if (!Pol) return (Polyhedron *)0;
  Pol1 = Polyhedron_Alloc(Pol->Dimension, Pol->NbConstraints, Pol->NbRays);
  /* Constraints and Rays may have space between them (from remove_red) */
  memcpy(Pol1->Constraint[0], Pol->Constraint[0],
    Pol->NbConstraints*(Pol->Dimension+2)*sizeof(int) );
  memcpy(Pol1->Ray[0], Pol->Ray[0],
    Pol->NbRays*(Pol->Dimension+2)*sizeof(int) );
  Pol1->NbBid = Pol->NbBid;
  Pol1->NbEq = Pol->NbEq;
  return Pol1;
}

Polyhedron *Domain_Copy(Pol)
Polyhedron *Pol;
{ Polyhedron *Pol1;
  if (!Pol) return (Polyhedron *) 0;
  Pol1 = Polyhedron_Copy(Pol);
  if (Pol->next) Pol1->next = Domain_Copy(Pol->next);
  return Pol1;
}

void Polyhedron_Free(Pol)
     Polyhedron *Pol;
{ free((char *) Pol->p_Init);
  free((char *) Pol->Constraint);
  free((char *) Pol);
} /* Polyhedron_Free */

void Domain_Free(Pol)
     Polyhedron *Pol;
{ if (Pol->next) Domain_Free(Pol->next);
  free((char *) Pol->p_Init);
  free((char *) Pol->Constraint);
  free((char *) Pol);
} /* Polyhedron_Free */

void Polyhedron_Print(Dst, Format, Pol)
     FILE *Dst;
     char *Format;
     Polyhedron *Pol;
{ unsigned Dimension, NbConstraints, NbRays;
  int      i, j, *p;

  if (!Pol)
  { fprintf(Dst, "<null polyhedron>\n");
    return;
  }
  Dimension     = Pol->Dimension + 2;  /* 1 for bid/uni, 1 for ray/pnt */
  NbConstraints = Pol->NbConstraints;
  NbRays        = Pol->NbRays;
  fprintf(Dst, "POLYHEDRON Dimension:%d\n", Pol->Dimension);
  fprintf(Dst,"           Constraints:%d  Equations:%d  Rays:%d  Lines:%d\n",
        Pol->NbConstraints, Pol->NbEq, Pol->NbRays, Pol->NbBid);

  fprintf(Dst,"Constraints %d %d\n", NbConstraints, Dimension);
  for (i=0;i<NbConstraints;i++) {
    p=Pol->Constraint[i];
    if (*p++) fprintf(Dst,"Inequality: [");
    else      fprintf(Dst,"Equality:   [");
    for (j=1;j<Dimension;j++)
      (void) fprintf(Dst, Format, *p++);
    (void) fprintf(Dst," ]\n");
  }

  (void)fprintf(Dst, "Rays %d %d\n", NbRays, Dimension);
  for (i=0;i<NbRays;i++) {
    p=Pol->Ray[i];
    if (*p++)
    {   if ( p[Dimension-2] ) fprintf(Dst, "Vertex: [");
        else                  fprintf(Dst, "Ray:    [");
    }
    else
    {                         fprintf(Dst, "Line:   [");
    }
    for (j=1; j < Dimension-1; j++) fprintf(Dst, Format, *p++);
    if (*p) fprintf(Dst, " ]/%d\n", *p);
    else    fprintf(Dst, " ]\n");
  }
  if (Pol->next)
  {  fprintf(Dst, "UNION ");
     Polyhedron_Print(Dst, Format, Pol->next);
  };
} /* Polyhedron_Print */

/* RaySort -- Sorts the rays (Ray, Sat) into three tiers:
         NbBid       <= i < equal_bound : saturates the constraint
         equal_bound <= i < sup_bound   : verifies the constraint
         sup_bound   <= i < NbRay       : does not verify
   the tier order is chosen so non-verifying rays can be easily removed 
*/
static void RaySort (Ray, Sat, NbBid, NbRay, equal_bound, sup_bound,
 RowSize1, RowSize2, Temp1, Temp2, bx, jx)
Matrix *Ray, *Sat;
int NbBid, NbRay, *equal_bound, *sup_bound;
unsigned RowSize1, RowSize2;
int *Temp1, *Temp2;
unsigned bx, jx;
{ 
  int inf_bound;
  int **uni_eq, **uni_sup, **uni_inf;
  int **inc_eq, **inc_sup, **inc_inf;

      /* sort rays : NbBid <= i < equal_bound : saturates the constraint
         : equal_bound <= i < sup_bound : verifies the constraint
         : sup_bound <= i < NbRay : does not verify */
      /* the order is chosen so non-verifying rays can be easily removed */

      *sup_bound = *equal_bound = NbBid;
      uni_sup = uni_eq = Ray->p+NbBid;
      inc_sup = inc_eq = Sat->p+NbBid;
      inf_bound = NbRay;
      uni_inf = Ray->p+NbRay;
      inc_inf = Sat->p+NbRay;

      while (inf_bound>*sup_bound)
      { if (**uni_sup==0)       /* status = satisfy */
        { bexchange(*uni_eq, *uni_sup, Temp1, RowSize1);
          bexchange(*inc_eq, *inc_sup, Temp2, RowSize2);
          (*equal_bound)++; uni_eq++; inc_eq++;
          (*sup_bound)++; uni_sup++; inc_sup++;
        }
        else
        { *((*inc_sup)+jx)|=bx;
          if (**uni_sup<0) {    /* status = verify */
            inf_bound--; uni_inf--; inc_inf--;
            bexchange(*uni_inf, *uni_sup, Temp1, RowSize1);
            bexchange(*inc_inf, *inc_sup, Temp2, RowSize2);
          }
          else                  /* status = does not verify */
          { (*sup_bound)++; uni_sup++; inc_sup++;
          }
        }
      }
}

/* Chernikova -- computes the dual of (Mat) and places it in (Ray).
   (Mat) contains the equations (one per row).
   (Ray) contains the lineality space and ray space,
   (NbBid) is the number of lines in (Ray).
   Processes constraints from (Mat) starting at (FirstConstraint)
   and modifies (Ray) to reflect the constraints,
   creates (Sat) a boolean matrix dimensioned by NbRay X NbCon
       defined as Sat(i,j)==0 if Ray(i) saturates Eqn(j), 1 otherwise
   Returns 0 if successful.
*/
static int Chernikova (Mat, Ray, Sat, NbBid, NbMaxRays, FirstConstraint)
     Matrix *Mat, *Ray, *Sat;
     unsigned NbBid, NbMaxRays, FirstConstraint;
{
  unsigned NbRay, Dimension, NbConstraints, RowSize1, RowSize2, nc;
  int *Temp1, *Temp2;
  int sup_bound, equal_bound, index_non_zero, bound;
  int i, j, k, l, aux, redundant, rayonly, nbcommonconstraints;
  unsigned bx, m, jx;
  int *p1, *p2, *p3;

  NbConstraints=Mat->NbRows;
  NbRay = Ray->NbRows;
  Dimension = Mat->NbColumns-1;         /* homogeneous dimension */
  nc=Sat->NbColumns;

  RowSize1=(Dimension+1) * sizeof(int);
  Temp1=(int *)malloc(RowSize1);
  RowSize2=nc * sizeof(int);
  Temp2=(int *)malloc(RowSize2);

  jx =   FirstConstraint/WSIZE;
  bx = MSB; bx >>= FirstConstraint%WSIZE;
  for (k=FirstConstraint; k<NbConstraints; k++)
  { 
    /* Set the status word of each ray[i] to ray[i] dot constraint[k] */
    /* This is equivalent to evaluating each ray by the constraint[k] */
    index_non_zero = NbRay;
    for (i=0; i<NbRay; i++)
    { p1 = Ray->p[i]+1;
      p2 = Mat->p[k]+1;
      p3 = Ray->p[i];
      *p3 = *p1++ * *p2++;
      for (j=1; j<Dimension; j++) *p3 += *p1++ * *p2++;
      if (*p3 && (i<index_non_zero)) index_non_zero=i;
    }
    /* finds a bidirectional ray z such that cz <> 0 */
    if (index_non_zero<NbBid)
    { /* discards index_non_zero bidirectional ray */
      NbBid--;
      if (NbBid!=index_non_zero)
        bexchange(Ray->p[index_non_zero], Ray->p[NbBid], Temp1, RowSize1);

      /* Computes the new lineality space */
      for (i=0; i<NbBid; i++)
        if (Ray->p[i][0]!=0)
          Combine(Ray->p[i], Ray->p[NbBid], Ray->p[i], 0, Temp1, Dimension);

      /* add the positive part of index_non_zero bidirectional ray to
         the set of unidirectional rays */
      if (Ray->p[NbBid][0]<0) {
        p1=Ray->p[NbBid]; 
        for (j=0; j<Dimension+1; j++) { *p1 = -(*p1); p1++; }
      }

      /* Computes the new pointed cone */
      for (i=NbBid+1; i<NbRay; i++)
        if (Ray->p[i][0]!=0)
          Combine(Ray->p[i], Ray->p[NbBid], Ray->p[i], 0, Temp1, Dimension);

      /* add the new ray */
      if (Mat->p[k][0]) /* Constraint is an inequality */
      { for (j=0; j<nc; j++) Sat->p[NbBid][j]=0; /* sat vec for new ray */
        /* new ray saturates everything except last inequality */
        Sat->p[NbBid][jx] |= bx;
      }
      else /* Constraint is an equality */
      { NbRay--;
        Vector_Copy(Ray->p[NbRay], Ray->p[NbBid], Dimension+1);
        Vector_Copy(Sat->p[NbRay], Sat->p[NbBid], nc);
      }
    } else {
      RaySort (Ray, Sat, NbBid, NbRay, &equal_bound, &sup_bound,
        RowSize1, RowSize2, Temp1, Temp2, bx, jx);
        /* sorts Uni into R0, R+, R- */
        /*        Ray 
NbRay-> bound-> ________
                |  R-  |        R- ==> ray.eq < 0  (outside domain)
          sup-> |------|
                |  R+  |        R+ ==> ray.eq > 0  (inside domain)
        equal-> |------|
                |  R0  |        R0 ==> ray.eq = 0  (on face of domain)
        NbBid-> |______|
        */
      /* Computes only the new pointed cone */
      bound=NbRay;
      for (i=equal_bound; i<sup_bound; i++)     /* for all pairs of */
        for(j=sup_bound; j<bound; j++) {        /* R- and R+ */
          /*-----------------------------------------------------------------*/
          /* compute and count the set of constraints saturated by R+ and R- */
          /* includes equalities, inequalities and the positivity constraint */
          /*-----------------------------------------------------------------*/
          nbcommonconstraints=0;
          for (l=0; l<jx; l++) {
            Temp2[l] = Sat->p[i][l] | Sat->p[j][l];
            aux=Temp2[l];
            for (m=MSB; m!=0; m>>=1) if (!(aux&m)) nbcommonconstraints++;
          }
          aux = Temp2[jx] = Sat->p[i][jx] | Sat->p[j][jx];
          for (m=MSB; m!=bx; m>>=1)
            if (!(aux&m)) nbcommonconstraints++;
          if (Ray->p[i][Dimension]==0 && Ray->p[j][Dimension]==0)
            nbcommonconstraints++;      /* account for pos constr */

          /*-----------------------------------------------------------------*/
          /* Adjacency Test : is combination [R-,R+] a non redundant ray?    */
          /*-----------------------------------------------------------------*/
          if (nbcommonconstraints+NbBid>=Dimension-2) /* dimensionality check*/
          { /* check whether a ray m saturates the same set of constraints */
            redundant=0;
            rayonly  =(Ray->p[i][Dimension]==0 && Ray->p[j][Dimension]==0);
            for (m=NbBid; m<bound; m++) if ((m!=i)&&(m!=j))
            { /* 2 rays (r+ r-) are never made redundant by a vertex   */
              /* because the positivity constraint saturates both rays */
              /* but not the vertex                                    */
              if (rayonly && Ray->p[m][Dimension]!=0) continue;

              /* (r+ r-) is redundant if there does NOT exist an equation */
              /* which saturates both r+ and r- but not rm */
              p1 = Temp2;
              p2 = Sat->p[m];
              for (l=0; l<=jx; l++,p2++,p1++) if (*p2&~*p1) break;
              if (l>jx)
              { redundant=1;
                break;
              }
            }

            /*------------------------------------------------------------*/
            /* Add new ray generated by [R+,R-]                           */
            /*------------------------------------------------------------*/
            if (!redundant)
            { if (NbRay==NbMaxRays)
              {
#ifndef NO_MESSAGES
                fprintf(stderr, "? chernikova: out of table space\n");
#endif
                return 1;
              }
              /* computes the new ray */
              Combine(Ray->p[j], Ray->p[i], Ray->p[NbRay],0,Temp1, Dimension);
              Vector_Or(Sat->p[j], Sat->p[i], Sat->p[NbRay], nc);
              Sat->p[NbRay][jx]&=~bx;
              NbRay++;
            }
          }
        }

      /* Eliminates all non extremal rays */
      j = (Mat->p[k][0]) ? sup_bound : equal_bound;
      i = NbRay;
      while ((j<bound)&&(i>bound)) {
        i--;
        Vector_Copy(Ray->p[i], Ray->p[j], Dimension+1);
        Vector_Copy(Sat->p[i], Sat->p[j], nc);
        j++;
      }
      if (j==bound) NbRay=i;
      else NbRay=j;
    }
    NEXT(jx, bx);
  }
  free(Temp1);
  free(Temp2);
  Ray->NbRows=NbRay;
  Sat->NbRows=NbRay;
  return 0;
} /* Chernikova */

/* Gauss --
   Computes a minimal system of equations.
   performs Gaussian elimination, detects contridiction.
   Given Mat, a matrix of constraints in which the first NbEq
      constraints are equations,
   Given NbEq, the number of equations in Mat
   Given Dimension, the dimension of the homonogenous system,
   Produce, col_of_rank, the vector of independent variables,
   Uses temp, a temporary vector,
   Returns the rank, if Rank=Dimension ... is there a contradiction
 */
int Gauss(Mat, NbEq, Dimension, temp)
Matrix *Mat;
int     NbEq, Dimension;
int     *temp;
{
  int RowSize = Dimension * sizeof(int);
  int *col_of_rank;
  int i, j, k, pivot, gcd, Rank, *cp;

  col_of_rank=(int *)malloc(RowSize);
  Rank=0;
  for (j=1; j<=Dimension; j++)         	/* for each column (except status) */ 
  { for (i=Rank; i<NbEq; i++)           /* starting at diagonal, look down */
      if (Mat->p[i][j]!=0) break;       /* to find the first non zero */
    if (i!=NbEq)                        /* was one found ? */
    { if (i!=Rank)                      /* was it found below the diagonal? */
          bexchange(Mat->p[Rank]+1, Mat->p[i]+1, temp, RowSize);

      /* Normalize the pivot row */
      if ((gcd=Vector_Gcd(Mat->p[Rank]+1, temp, Dimension))>=2 )
      { cp=&Mat->p[Rank][1];
        for (k=0; k<Dimension; k++) *cp++/=gcd;
      }
      if (Mat->p[Rank][j]<0)
      { cp = Mat->p[Rank]+1;
        for (k=0; k<Dimension; k++) *cp++ *=-1;
      }
      /* end normalize */

      pivot=i;
      for (i=pivot+1; i<NbEq; i++)      /* zero out the rest of the column */
      { if (Mat->p[i][j]!=0)
          Combine(Mat->p[i], Mat->p[Rank], Mat->p[i], j, temp, Dimension);
      }
      col_of_rank[Rank]=j;
      Rank++;
    }
  } /* end of Gauss elimination */

  /* Back Substitution -- normalizes the system of equations */
  for (k=Rank-1; k>=0; k--)
  { j = col_of_rank[k];
    /* Normalize the equations */
    for (i=0; i<k; i++)
    { if (Mat->p[i][j]!=0 )
        Combine(Mat->p[i], Mat->p[k], Mat->p[i], j, temp, Dimension);
    }
    /* Normalize the inequalities */
    for (i=NbEq; i<Mat->NbRows; i++)
    { if (Mat->p[i][j]!=0 )
        Combine(Mat->p[i], Mat->p[k], Mat->p[i], j, temp, Dimension);
    }
  }
  free(col_of_rank);
  return Rank;
}

/* Empty_Polyhedron --
   Create and return an empty polyhedron of non-homo dimension 'Dimension'
*/
Polyhedron *Empty_Polyhedron(Dimension)
  unsigned  Dimension;  /* non-homogeneous dimension */
{ Polyhedron *Pol;
  int i;
  /* create an overconstrained system of equations:
        x=0, y=0, ... , z=0, 1=0
     and no rays
     Dimension of Ray Space = Dimension - NbBid - NbEq = -1 (empty)
  */
  Pol = Polyhedron_Alloc(Dimension, /* NbEq */ Dimension+1, /* NbRay */ 0);
  Vector_Init(Pol->Constraint[0], (Dimension+1)*(Dimension+2));
  for (i=0; i<=Dimension; i++)
    Pol->Constraint[i][i+1]=1;
  Pol->NbEq = Dimension+1;
  Pol->NbBid = 0;
  return Pol;
} /* Empty_Polyhedron */

/* Universe_Polyhedron --
   Create and return an universe polyhedron of non-homo dimension 'Dimension'
*/
Polyhedron *Universe_Polyhedron(Dimension)
  unsigned  Dimension;  /* non-homogeneous dimension */
{ Polyhedron *Pol;
  int i;
  /* create an unconstrained system of equations:
        1>=0
     and vertex(0,0,0...) and line (1,0,0...) , (0,1,0,... ) and (0,0,1,...)
     Dimension of Ray Space = Dimension - NbBid - NbEq = 0
  */
  Pol = Polyhedron_Alloc(Dimension, /* NbIneq */ 1, /* NbRay */ Dimension+1);

  Vector_Init(Pol->Constraint[0], (Dimension+2));
  Pol->Constraint[0][0] = 1;
  Pol->Constraint[0][Dimension+1] = 1;

  Vector_Init(Pol->Ray[0], (Dimension+1)*(Dimension+2));
  for (i=0; i<=Dimension; i++)
    Pol->Ray[i][i+1]=1;
  Pol->Ray[Dimension][0] = 1;  /* vertex status */
  Pol->NbEq = 0;
  Pol->NbBid = Dimension;
  return Pol;
} /* Universe_Polyhedron */

/* Remove_Redundants --
   Given (Mat) a matrix of equations and inequalities, and
   given (Ray) a matrix of rays and lines
   given (Sat) the saturation matrix,
   Return a polyhedron, composed of Mat and Ray after reductions.
   Usually called as a follup to Chernikova to remove redundant
   equations.  (Chernikova ensures that there are no redundant
   lines and rays).
*/
static Polyhedron *Remove_Redundants(Mat, Ray, Sat, Filter)
Matrix   *Mat, *Ray, *Sat;
unsigned        *Filter;
{ int i, j, k;
  unsigned Dimension, nc, NbRay, NbConstraints, RowSize1, RowSize2,
           *Trace, *bx, *jx, Dim, Status, b;
  unsigned NbBid, NbUni, NbEq, NbIneq, NbBid2, NbUni2, NbEq2, NbIneq2;
  int Redundant;
  int aux, *p, *q, *temp1, *temp2;
  Polyhedron *Pol;

  Dimension     = Mat->NbColumns-1;     /* homogeneous dimension */

  /* abort on error from Chernikova */
  if (Pol_status) return Empty_Polyhedron(Dimension-1);

  NbRay         = Ray->NbRows;
  nc            = Sat->NbColumns;
  NbConstraints = Mat->NbRows;
  RowSize1=(Dimension+1) * sizeof(int);
  RowSize2=nc*sizeof(int);
  temp1=(int *)malloc(RowSize1);
  temp2=(int *)malloc(RowSize2);

  /* introduce indirections into Sat matrix to simplify processing
     with Sat and allow easy exchanges of columns */
  bx = (unsigned *)malloc(NbConstraints * sizeof(unsigned));
  jx = (unsigned *)malloc(NbConstraints * sizeof(unsigned));
  i = 0;
  b = MSB;
  for (j=0; j<NbConstraints; j++)
  { jx[j] = i;
    bx[j] = b;
    NEXT(i,b);
  }

  /* reset the status numbers, count the vertices */
  aux = 0;
  for (i=0; i<NbRay; i++)
  {  Ray->p[i][0]=0;
     if (Ray->p[i][Dimension]!=0) aux++;        /* vertex */
  }
  if (!aux)     /* no vertices -- empty polyhedron */
  {   /* return an empty polyhedron */
      free(temp1); free(temp2); free(jx); free(bx);
      return Empty_Polyhedron(Dimension-1);
  }

  /* Step 1: Compute status counts for both rays and inequalities */
  /* for each equation, count the number of vertices/rays saturated
     by that equation, put the result in the status words.
     At the same time, for each vertex/ray, count the number of
     constaints saturated by it. */
  NbEq=0;
  memset((char *)temp2, 0, RowSize2);                   /* for SIMPLIFY */
  for (j=0; j<NbConstraints; j++)
  { if (Filter && !Mat->p[j][0]) temp2[jx[j]] |= bx[j]; /* for SIMPLIFY */
    Mat->p[j][0]=0;

    /* identify and remove the positivity constraint 1>=0 */
    for (i=1, p = &Mat->p[j][1]; i<Dimension; i++) if (*p++) break;
    if (i==Dimension) /* Constraint j is a positivity constraint */
    {   /* or is it 1==0 ??   -- lets check */
        for (i=0; i<NbRay; i++)
            if (!(Sat->p[i][jx[j]]&bx[j])) Mat->p[j][0]++;
        if ((Mat->p[j][0]==NbRay) &&   /* it is an equality */
            (Mat->p[j][Dimension]!=0)) /* and its not 0=0 */
        {   /* return an empty polyhedron */
            free(temp1); free(temp2); free(jx); free(bx);
            return Empty_Polyhedron(Dimension-1);
        }

        /* delete the positivity constraint */
        NbConstraints--;
        if (j==NbConstraints) continue;
        bexchange(Mat->p[j], Mat->p[NbConstraints], temp1, RowSize1);
        exchange(jx[j], jx[NbConstraints], aux);
        exchange(bx[j], bx[NbConstraints], aux);
        j--; continue;
    }

    /* count the incidences of saturations */
    for (i=0; i<NbRay; i++) if (!(Sat->p[i][jx[j]]&bx[j]))
    {  Mat->p[j][0]++;
       Ray->p[i][0]++;
    }
    if (Mat->p[j][0]==NbRay) NbEq++;    /* all vert/rays are saturated */
  }
  Mat->NbRows = NbConstraints;

  NbBid=0;
  for (i=0; i<NbRay; i++)
  { /* give rays credit for saturating the positivity constraint */
    if (Ray->p[i][Dimension]==0) Ray->p[i][0]++;
    /* test for a line.  Note that a vertex cannot become a line because
       only rays satisfy the pos constr and can status == NbConstraints+1 */
    if (Ray->p[i][0]==NbConstraints+1) NbBid++;
  }

  /* Step2: Sort equations to top of Mat, inequalities afterward */
  /* Detection of implicit equations such as y>=3; y<=3 */
  /* Keep inequalities in same relative order for SIMPLIFY to work */
  for (i=0; i<NbEq; i++)
    if (Mat->p[i][0]!=NbRay) /* vert/rays are NOT all saturated (inequality)*/
    { for (k=i+1; Mat->p[k][0]!=NbRay && k<NbConstraints; k++)
        /* skip over inequalities -- find an equation */ ;
      if (k==NbConstraints) /* none found--program error (NbEq wrong)*/ break;
      /* slide inequalities up, move equality down (toward begining of array)*/
      memcpy(temp1, Mat->p[k], RowSize1);
      aux = jx[k];
      j   = bx[k];
      for (;k>i;k--)
      {  memcpy(Mat->p[k], Mat->p[k-1], RowSize1);
         jx[k] = jx[k-1];
         bx[k] = bx[k-1];
      }
      memcpy(Mat->p[i], temp1, RowSize1);
      jx[i] = aux;
      bx[i] = j;
    }

  if (Filter)                     /* for SIMPLIFY */
  for (i=0; i<NbEq; i++)
  {  Redundant = 0;
     for (j=i+1; j<NbEq; j++)
     {  for (k=0, p=&Mat->p[i][1], q=&Mat->p[j][1]; k<Dimension; k++,p++,q++)
        {  if (*p!=*q) break;
        }
        /* Redundant if both the same `and' Mat[j] was equation (temp2==1) */
        if (k==Dimension && (temp2[jx[j]] & bx[j])) { Redundant=1; break; }
     }
     if (!Redundant) Filter[jx[i]] |= bx[i];  /* set flag */
  }

  /* Step 3: Gaussian elimiation of Equalities and reduction of
             strength on rays */ 
  NbEq2 = Gauss(Mat, NbEq, Dimension, temp1);
  if (NbEq2>=Dimension)		/* there is a contradiction */
  {  /* return an empty polyhedron */
     free(temp1); free(temp2); free(jx); free(bx);
     return Empty_Polyhedron(Dimension-1);
  }

  /* Step 4: Sort lines to top of Ray, rays afterward */
  /* Detection of implicit lines such as ray(1,2) and ray(-1,-2) */
  for (i=0, k=NbRay; i<NbBid && k>i; i++)
    if (Ray->p[i][0]!=(NbConstraints+1) ) /* all constraints not saturated */
    { while (--k >i && Ray->p[k][0]!=(NbConstraints+1)) ;
      bexchange(Ray->p[i], Ray->p[k], temp1, RowSize1);
      bexchange(Sat->p[i], Sat->p[k], temp2, RowSize2);
    }

  /* Step 5 : Gaussian Elimination of on Lineality Space
              and computation of dimension of Ray Space */
  NbBid2 = Gauss(Ray, NbBid, Dimension, temp1);
  if (NbBid2>=Dimension)
  {
#ifndef NO_MESSAGES
     fprintf(stderr, "? RemoveRedundants: dimensional error\n");
#endif
     Pol_status = 1;
     free(temp1); free(temp2); free(jx); free(bx);
     return Empty_Polyhedron(Dimension-1);
  }
  /* compute dimension of the ray space */
  Dim = Dimension-1-NbEq2-NbBid2;  /* Dim of non-homogen Ray Space */

  /* Step 6: First pass filter of inequalities and equation identification */
  NbIneq=0;
  for (j=0; j<NbConstraints; j++)
  { /* New positivity constraints may have been created by step 3.
       the Gaussian elimination -- must check for them */
    /* identify and remove the positivity constraint 1>=0 */
    for (i=1, p = &Mat->p[j][1]; i<Dimension; i++) if (*p++) break;
    if (i==Dimension) /* Constraint j is a positivity constraint */
    {   /* or is it 1==0 ??   -- lets check */
        if ((Mat->p[j][0]==NbRay) &&   /* it is an equality */
            (Mat->p[j][Dimension]!=0)) /* and its not 0=0 */
        {   /* return an empty polyhedron */
            free(temp1); free(temp2); free(jx); free(bx);
            return Empty_Polyhedron(Dimension-1);
        }

        /* set the positivity constraint redundant */
        Mat->p[j][0]=2;
        continue;
    }

    if     ((Status=Mat->p[j][0])==0) Mat->p[j][0]=2;  /* redundant */
    else if (Status<Dim)              Mat->p[j][0]=2;  /* redundant */
    else if (Status==NbRay)           Mat->p[j][0]=0;  /* equality */
    else                  { NbIneq++; Mat->p[j][0]=1;} /* inequality */
  }

  /* Step 7: First pass filter of rays, identification of lines */
  /*    If Dim=0, the origin in the universe will have status 0-- but don't
        delete it, please */
  NbUni=0;
  for (j=0; j<NbRay; j++)
  { if      ((Status=Ray->p[j][0])<Dim) Ray->p[j][0]=2;  /* redundant */
    else if (Status==(NbConstraints+1)) Ray->p[j][0]=0;  /* line */
    else                     { NbUni++; Ray->p[j][0]=1;} /* ray */
  }

  /* Allocate the polyhedron (using approximate sizes) */
  Pol = Polyhedron_Alloc(Dimension-1, NbIneq+NbEq2+1, NbUni+NbBid2);
  Pol->NbBid = NbBid2;
  Pol->NbEq  = NbEq2;

  /* Partially fill the Polyhedron structure */
  if (NbBid2) Vector_Copy(Ray->p[0], Pol->Ray[0], (Dimension+1)*NbBid2);
  if (NbEq2)  Vector_Copy(Mat->p[0], Pol->Constraint[0], (Dimension+1)*NbEq2);

  /* Step 8: Final pass filter of inequalities */
  /* Detection of redundants inequalities */
  /* redundant inequalitIes will include:
        1. inequalities which are always true, such as 1>=0,
        2. redundant inequalities such as y>=4 given y>=3, or
           x>=2 given x=1, etc.
        3. redundant inequalities such as x+y>=5 given x>=3 and y>=2 */
  /* every 'good' inequality must saturate at least Dimension rays 
     and be unique */
  Trace=(unsigned *)malloc(nc * sizeof(unsigned));
  /* 			j-->
			_________________
			|		|
			|      Mat	|
			|_______________|
				    |
	_________	____________V____
      i	|	|	|	    0	|
      |	|  Ray	|	|      Sat	|
      V	|	|	|	    0	|
	|	|	|	    0	|
	|_______|	|_______________|
			       -OR-
			________|________
			|_____Trace_0___|
  */
  NbIneq2 = 0;
  for (j=NbEq; j<NbConstraints; j++)
  { if (Mat->p[j][0]==1)	/* non-redundant inequality */
    { for (k=0; k<nc; k++) Trace[k]=0;		/* init Trace */
      /* compute Trace: the union of all rows of Sat where
			constraint j is saturated */
      for (i=NbBid; i<NbRay; i++) if (Ray->p[i][0]==1)
      { if (!(Sat->p[i][jx[j]]&bx[j])) 
          for (k=0; k<nc; k++) Trace[k] |= Sat->p[i][k];
      }

      /* only constraint j should saturate this set of vertices/rays.
	 if another non-redundant constraint also saturates this set,
         then constraint j is redundant */
      Redundant=0;
      for (i=NbEq; i<NbConstraints; i++) {
        if ( (Mat->p[i][0] ==1) && (i!=j) && !(Trace[jx[i]] & bx[i]) )
        {   Redundant=1;
            break;
        }
      }
      if (Redundant) Mat->p[j][0]=2;
      else
      { Vector_Copy(Mat->p[j], Pol->Constraint[NbEq2+NbIneq2], Dimension+1);
        if (Filter) Filter[jx[j]] |= bx[j];		/* for SIMPLIFY */
	NbIneq2++;
      }
    }
  }
  free(Trace);

  /* Step 9: Final pass filter of rays */
  /* Detection of redundants rays */
  Trace=(unsigned *)malloc(NbRay * sizeof(int));
  /* 			j-->
			_________________
			|		|
			|      Mat	|
			|_______________|

	_________	_________________  ___
      i	|	|	|	 	|  |T|
      |	|  Ray	|	|      Sat	|  |r|
      V	|	|	|	 	|  |a|
	|	|	|	 	|  |c|
	|	|------>|  0   0 0   0	|  |e|
	|_______|	|_______________|  |_|
  */
  NbUni2 = 0;
  aux = 0;	/* let aux be the number of rays not vertices */
  for (i=NbBid; i<NbRay; i++) {
    if (Ray->p[i][0]==1)
    { if (Ray->p[i][Dimension]!=0)	/* vertex */
          for (k=NbBid; k<NbRay; k++) Trace[k]=0;	/* init Trace */
      else				/* ray */
	  /* include the positivity constraint incidences for rays:
             the positivity constraint saturates all rays and no vertices */
	  for (k=NbBid; k<NbRay; k++) Trace[k]=(Ray->p[k][Dimension]!=0);

      Redundant=0;
      { /* compute Trace: the union of all cols of Sat where
			  ray i is saturated */
        for (j=NbEq; j<NbConstraints; j++) if (Mat->p[j][0]==1) /* ineq */
        { if (!(Sat->p[i][jx[j]]&bx[j]))
            for (k=NbBid; k<NbRay; k++) Trace[k] |= Sat->p[k][jx[j]]&bx[j];
        }

        /* if ray i does not saturate any inequalities (other than
	   the positivity constraint, then it is the case that there is
	   only one inequality and that ray is its orthogonal */

        /* only ray i should saturate this set of inequalities.
	   if another non-redundant ray also saturates this set,
           then ray i is redundant */
        for (j=NbBid; j<NbRay; j++)
        { if ( (Ray->p[j][0]==1) && (i!=j) && !Trace[j] )
          {   Redundant=1;
              break;
          }
	}
      }
      if (Redundant) Ray->p[i][0]=2;
      else
      { Vector_Copy(Ray->p[i], Pol->Ray[NbBid2+NbUni2], Dimension+1);
	NbUni2++;			    /* inc nb of rays */
	if (Ray->p[i][Dimension]==0) aux++; /* inc nb of rays not vertices */
      }
    }
  }
  if (aux>=Dim) /* include the positivity constraint */
  { Vector_Init(Pol->Constraint[NbEq2+NbIneq2], Dimension+1);
    Pol->Constraint[NbEq2+NbIneq2][0] = 1;		/* inequality */
    Pol->Constraint[NbEq2+NbIneq2][Dimension] = 1;	/* 1>=0 */
    NbIneq2++;
  }

  free(Trace);
  free(bx);
  free(jx);
  free(temp2);
  free(temp1);

  Pol->NbConstraints = NbEq2 + NbIneq2;
  Pol->NbRays = NbBid2 + NbUni2;
  return Pol;

} /* Remove_Redundants */

Polyhedron *Constraints2Polyhedron(Constraints, NbMaxRays)
     Matrix *Constraints;
     unsigned NbMaxRays;
{
  Polyhedron *Pol;
  Matrix *Ray, *Sat;
  unsigned Dimension, nc;
  int i;

  Dimension     = Constraints->NbColumns - 1;	/* homogeneous dimension */
  if (Constraints->NbRows==0)
  {  Pol = Universe_Polyhedron(Dimension-1);
     return Pol;
  }
  nc            = ( Constraints->NbRows - 1 ) / (sizeof(int)*8) + 1;

  /* rather than add a 'positivity constraint', it is better to
     initialize the lineality space with line in each of the index
     dimensions, but no line in the lambda dimension.   Then initialize
     the ray space with an origin at 0.  This is what you get anyway,
     after the positivity constraint has been processed by Chernikova. */

  /* Initialize Ray Space to Universe */
  Ray = Matrix_Alloc(NbMaxRays, Dimension+1);
  Vector_Init(Ray->p_Init, Dimension*(Dimension+1) );
  for (i=0; i<Dimension; i++) Ray->p[i][i+1]=1;
  Ray->p[Dimension-1][0] = 1;		/* mark for ray */
  Ray->NbRows = Dimension; 

  Sat = Matrix_Alloc(NbMaxRays, nc);
  Vector_Init(Sat->p_Init, Dimension*nc );
  Sat->NbRows = Dimension;

  Pol_status =
  Chernikova(Constraints, Ray, Sat, /* NbBid */ Dimension-1, NbMaxRays, 0);
  /* remove redundant equations and build the polyhedron */
  Pol = Remove_Redundants(Constraints, Ray, Sat, 0);

  Matrix_Free(Sat);
  Matrix_Free(Ray);
  return Pol;
} /* Constraints2Polyhedron */

  /* Transpose Sat --> Sat2 */
Matrix *TransformSat(Mat, Ray, Sat)
Matrix *Mat, *Ray, *Sat;
  { int i, j, nc2;
    unsigned jx1, jx2, bx1, bx2;
    Matrix *Sat2;

    if (Mat->NbRows != 0) nc2 = (Mat->NbRows-1) / (sizeof(int)*8) + 1;
    else                  nc2 = 0;
    Sat2 = Matrix_Alloc(Ray->NbRows, nc2);
    Vector_Init(Sat2->p_Init, Ray->NbRows*nc2);
    for (i=0, jx1=0, bx1=MSB; i<Ray->NbRows; i++)
    { for(j=0, jx2=0, bx2=MSB; j<Mat->NbRows; j++)
      { if (Sat->p[j][jx1]&bx1) Sat2->p[i][jx2] |= bx2;
        NEXT(jx2,bx2);
      }
      NEXT(jx1, bx1);
    }
    return Sat2;
  }


Polyhedron *Rays2Polyhedron(Ray, NbMaxRays)
     Matrix     *Ray;
     unsigned   NbMaxRays;
{
  Polyhedron *Pol;
  Matrix *Mat, *Sat, *Sat2;
  unsigned Dimension, nc;
  int i;

  Dimension     = Ray->NbColumns-1;	/* homogeneous dimension */
  if (Ray->NbRows==0)
  {  Pol = Empty_Polyhedron(Dimension-1);
     return(Pol);
  }
  nc            = ( Ray->NbRows -1 ) / (sizeof(int)*8) + 1;

  /* Initialize Mat to Empty Polyhedron */
  Mat = Matrix_Alloc(NbMaxRays, Dimension+1);
  Vector_Init( Mat->p_Init, Dimension*(Dimension+1) );
  for (i=0; i<Dimension; i++) Mat->p[i][i+1]=1;
  Mat->NbRows = Dimension;

  Sat  = Matrix_Alloc(NbMaxRays, nc);
  Vector_Init(Sat->p_Init, Dimension*nc );
  Sat->NbRows = Dimension;

  Pol_status =
  Chernikova(Ray, Mat, Sat, /* NbEq */ Dimension, NbMaxRays, 0);
  Sat2 = TransformSat(Mat, Ray, Sat);
  Matrix_Free(Sat);

  Pol = Remove_Redundants(Mat, Ray, Sat2, 0);

  Matrix_Free(Sat2);
  Matrix_Free(Mat);
  return Pol;
} /* Rays2Polyhedron */

Matrix *Polyhedron2Rays(Pol)
     Polyhedron *Pol;
{
  Matrix     *Ray;
  unsigned NbRays, Dimension;
  NbRays    = Pol->NbRays;
  Dimension = Pol->Dimension+2;		/* homogeneous Dimension + Status */
  Ray = Matrix_Alloc(NbRays, Dimension);
  Vector_Copy(Pol->Ray[0], Ray->p_Init, NbRays*Dimension);
  return Ray;
} /* Polyhedron2Rays */

Matrix *Polyhedron2Constraints(Pol)
     Polyhedron *Pol;
{
  Matrix     *Mat;
  unsigned NbConstraints, Dimension;
  NbConstraints = Pol->NbConstraints;
  Dimension     = Pol->Dimension+2;
  Mat = Matrix_Alloc(NbConstraints, Dimension);
  Vector_Copy(Pol->Constraint[0], Mat->p_Init, NbConstraints*Dimension);
  return Mat;
} /* Polyhedron2Constraints */

static Matrix *BuildSat(Mat, Ray, NbCon, NbMaxRays)
Matrix *Mat, *Ray;
unsigned NbCon, NbMaxRays;
{   Matrix *Sat;
    int i, j, k, jx, *p1, *p2, *p3;
    unsigned Dimension, NbRay, bx, nc;

    NbRay = Ray->NbRows;
    Dimension = Mat->NbColumns-1;	/* homogeneous Dimension */
 
    /* build the Sat matrix */
    /* at same time, verify the integrity of the polyhedron */
    nc      = (Mat->NbRows - 1) / (sizeof(int)*8) + 1;
    Sat     = Matrix_Alloc(NbMaxRays, nc);
    Sat->NbRows = NbRay;
    Vector_Init(Sat->p_Init, nc*NbRay);
    jx=0; bx=MSB;
    for (k=0; k<NbCon; k++)
    {   for (i=0; i<NbRay; i++)
        {   p1 = Ray->p[i]+1;
            p2 = Mat->p[k]+1;
            p3 = Ray->p[i];
            *p3 = *p1++ * *p2++;
            for (j=1; j<Dimension; j++) *p3 += *p1++ * *p2++;
        }
        for (j=0; j<NbRay; j++)
        {  if (Ray->p[j][0]) Sat->p[j][jx]|=bx;
        }
        NEXT(jx, bx);
    }
    return Sat;
}

Polyhedron *AddConstraints(Con2, NbCon2, Pol1, NbMaxRays)
     int	*Con2;
     unsigned	NbCon2;
     Polyhedron *Pol1;
     unsigned   NbMaxRays;
{
  Polyhedron *Pol;
  Matrix   *Mat, *Ray, *Sat;
  unsigned NbRay, NbCon, NbCon1, NbEle1, Dimension;

  NbRay		= Pol1->NbRays;
  NbCon1	= Pol1->NbConstraints;
  NbCon		= NbCon1 + NbCon2;
  Dimension	= Pol1->Dimension+2;	/* homogeneous Dimension + Status */
  NbEle1	= NbCon1*Dimension;

  Mat = Matrix_Alloc(NbCon, Dimension);

  /* Set the constraints of Pol1 */
  Vector_Copy(Pol1->Constraint[0], Mat->p_Init, NbEle1);

  /* Add the new constraints */
  Vector_Copy(Con2, Mat->p_Init+NbEle1, NbCon2*Dimension);

  Ray = Matrix_Alloc(NbMaxRays, Dimension);
  Ray->NbRows = NbRay;
  Vector_Copy(Pol1->Ray[0], Ray->p_Init, NbRay*Dimension);

  Sat = BuildSat(Mat, Ray, NbCon1, NbMaxRays);
  Pol_status =
  Chernikova(Mat, Ray, Sat, Pol1->NbBid, NbMaxRays, NbCon1);
  Pol = Remove_Redundants(Mat, Ray, Sat, 0);

  Matrix_Free(Sat);
  Matrix_Free(Ray);
  Matrix_Free(Mat);
  return Pol;
} /* AddConstraints */

/* addToFilter -- Used for the DomSimplify procedure */
static void addToFilter(k, Filter, Sat, tmpR, tmpC, NbRays, NbConstraints)
int k, NbRays, NbConstraints;
unsigned *Filter;
int *tmpR, *tmpC;
Matrix *Sat;
{   int kj, i,j, jx;
    unsigned kb, bx;

    /* remove constraint k */
    kj =   k/WSIZE; kb = MSB; kb >>= k%WSIZE;
    Filter[kj]|=kb;
    tmpC[k]=-1;
    /* (*NbConstraintsLeft)--; */

    /* remove rays excluded by constraint k */
    for(i=0; i<NbRays; i++) if (tmpR[i]>=0)
    {   if (Sat->p[i][kj]&kb) tmpR[i]--;  /* adjust included ray */
        else
        {   /* constraint k excludes ray i -- delete ray i */
            tmpR[i]=-1;
            /* (*NbRaysLeft)--; */

            /* adjust non-deleted constraints */
            jx=0; bx=MSB;
            for(j=0; j<NbConstraints; j++)
            {   if ((tmpC[j]>=0)&&(Sat->p[i][jx]&bx)) tmpC[j]--;
                NEXT(jx,bx);
            }
        }
    }
}

/* FindSimple --
   P1 and P2 is empty: what is the minimum set of constraints in P1
   that, when intersected with P2, gives the empty set */
static void     FindSimple(P1, P2, Filter, NbMaxRays)
Polyhedron *P1, *P2;
int *Filter;
unsigned NbMaxRays;
{   Matrix *Mat, *Sat;
    int i, j, k, jx, *p1, *p2, p3, found;
    unsigned Dimension, NbRays, NbConstraints, NbConstraintsLeft, bx, nc;
    int *tmpC, *tmpR;
    Polyhedron *Pol, *Pol2;

    Dimension = P1->Dimension+2;       /* status + homogeneous Dimension */
    Mat = Matrix_Alloc(P1->NbConstraints, Dimension);

    /* post constraints in P1 already included by Filter */
    jx = 0; bx = MSB; Mat->NbRows=0; NbConstraintsLeft=0;
    for (k=0; k<P1->NbConstraints; k++)
    {   if (Filter[jx]&bx)
        {  Vector_Copy(P1->Constraint[k], Mat->p[Mat->NbRows], Dimension);
           Mat->NbRows++;
        }
        else NbConstraintsLeft++;
        NEXT(jx,bx);
    }
    Pol2 = P2;

    for (;;)
    {   if (Mat->NbRows==0) Pol = Polyhedron_Copy(Pol2);
        else
        {   Pol = AddConstraints(Mat->p_Init, Mat->NbRows, Pol2, NbMaxRays);
            if (Pol2 != P2) Polyhedron_Free(Pol2);
        }
        if (emptyQ(Pol))
        {   Matrix_Free(Mat);
            Polyhedron_Free(Pol);
            return;
        }
        Mat->NbRows = 0;        /* Reset Mat */
        Pol2 = Pol;

        /* Its not enough-- find some more constraints */
        Dimension         = Pol->Dimension+1;       /* homogeneous Dimension */
        NbRays            = Pol->NbRays;
        NbConstraints     = P1->NbConstraints;
        tmpR = (int *)malloc(NbRays*sizeof(int));
        tmpC = (int *)malloc(NbConstraints*sizeof(int));
        memset((char *)tmpR, 0, NbRays*sizeof(int));
        memset((char *)tmpC, 0, NbConstraints*sizeof(int));

        /* build the Sat matrix */
        nc      = (NbConstraints - 1) / 32 + 1;
        Sat     = Matrix_Alloc(NbRays, nc);
        Sat->NbRows = NbRays;
        Vector_Init(Sat->p_Init, nc*NbRays);

        jx=0; bx=MSB;
        for (k=0; k<NbConstraints; k++)
        {   if (Filter[jx]&bx) tmpC[k]=-1;
            else for (i=0; i<NbRays; i++)
            {   p1 = Pol->Ray[i]+1;
                p2 = P1->Constraint[k]+1;
                p3 = *p1++ * *p2++;
                for (j=1; j<Dimension; j++) p3 += *p1++ * *p2++;
                if (p3==0 || (p3>0 && P1->Constraint[k][0]))
                {   Sat->p[i][jx]|=bx;  /* constraint includes ray, set flag */
                    tmpR[i]++;
                    tmpC[k]++;
                }
            }
            NEXT(jx, bx);
        }

        do /* find all of the essential constraints */
        {   found = 0;
            for(i=0; i<NbRays; i++) if (tmpR[i]>=0)
            {   if (tmpR[i]+1 == NbConstraintsLeft)
                {   /* Ray i is excluded by only one constraint... find it */
                    jx = 0; bx = MSB;
                    for(k=0; k<NbConstraints; k++)
                    {   if((tmpC[k]>=0) && ((Sat->p[i][jx]&bx)==0))
                        {   addToFilter(k, Filter, Sat, tmpR, tmpC,
                                        NbRays, NbConstraints);
                            Vector_Copy(P1->Constraint[k],
                                        Mat->p[Mat->NbRows], Dimension+1);
                            Mat->NbRows++;
                            NbConstraintsLeft--;
                            found=1;
                            break;
                        }
                        NEXT(jx,bx);
                    }
                    break;
                }
            }
        } while (found);

        if (!Mat->NbRows) /* Well then, just use a stupid heuristic */
        {   /* find the constraint which excludes the most */
            i = MSB;
            i = ~i;
            j = -1;
            for(k=0; k<NbConstraints; k++) if (tmpC[k]>=0)
            {   if (i>tmpC[k])
                {   i = tmpC[k];
                    j = k;
                }
            }
            if (j<0)
            {
#ifndef NO_MESSAGES
                fprintf(stderr, "? DomSimplify logic error.\n");
#endif
                Pol_status=1;
            }
            else    
            {   addToFilter(j, Filter, Sat, tmpR, tmpC, NbRays, NbConstraints);
                Vector_Copy(P1->Constraint[j],Mat->p[Mat->NbRows],Dimension+1);
                Mat->NbRows++;
                NbConstraintsLeft--;
            }
        }
        Matrix_Free(Sat);
        free(tmpC);
        free(tmpR);
    }
}

/* return 1 if intersection of Pol1 and Pol2 is not empty */
static int SimplifyConstraints(Pol1, Pol2, Filter, NbMaxRays)
     Polyhedron *Pol1, *Pol2;
     unsigned	*Filter;
     unsigned   NbMaxRays;
{
  Polyhedron *Pol;
  Matrix   *Mat, *Ray, *Sat;
  unsigned NbRay, NbCon, NbCon1, NbCon2, NbEle1, Dimension, notvid;

  NbRay         = Pol1->NbRays;
  NbCon1        = Pol1->NbConstraints;
  NbCon2	= Pol2->NbConstraints;
  NbCon         = NbCon1 + NbCon2;
  Dimension     = Pol1->Dimension+2;    /* homogeneous Dimension + Status */
  NbEle1        = NbCon1*Dimension;

  Mat = Matrix_Alloc(NbCon, Dimension);

  /* Set the constraints of Pol1 */
  Vector_Copy(Pol1->Constraint[0], Mat->p_Init, NbEle1);

  /* Add the new constraints */
  Vector_Copy(Pol2->Constraint[0], Mat->p_Init+NbEle1, NbCon2*Dimension);

  Ray = Matrix_Alloc(NbMaxRays, Dimension);
  Ray->NbRows = NbRay;
  Vector_Copy(Pol1->Ray[0], Ray->p_Init, NbRay*Dimension);

  Sat = BuildSat(Mat, Ray, NbCon1, NbMaxRays);
  Pol_status =
  Chernikova(Mat, Ray, Sat, Pol1->NbBid, NbMaxRays, NbCon1);
  Pol = Remove_Redundants(Mat, Ray, Sat, Filter);
  notvid = 1;
  if (Filter && emptyQ(Pol) ) 
  {  notvid = 0;
     FindSimple(Pol1, Pol2, Filter, NbMaxRays);
  }
  Polyhedron_Free(Pol);
  Matrix_Free(Sat);
  Matrix_Free(Ray);
  Matrix_Free(Mat);
  return notvid;
} /* SimplifyConstraints */

/* SubConstraint--
   Add the inverse of Con2 to Pol1
   Pass : 0 add ( -constraint -1 )
          1 add ( +constraint -1 )
	  2 add ( -constraint    )
	  3 add (  constraint    )
*/
Polyhedron *SubConstraint(Con2, Pol1, NbMaxRays, Pass)
     int	*Con2,Pass;
     Polyhedron *Pol1;
     unsigned   NbMaxRays;
{ Polyhedron *Pol;
  Matrix   *Mat, *Ray, *Sat;
  unsigned NbRay, NbCon, NbCon1, NbEle1, Dimension;
  int      i, j;

  /* if Con2 is the positivity constraint -- check out now */
  Dimension     = Pol1->Dimension+1;    /* homogeneous Dimension */
  for (i=1; i<Dimension; i++) if (Con2[i]) break;
  if (i==Dimension) return (Polyhedron *)0;

  NbRay		= Pol1->NbRays;
  NbCon1	= Pol1->NbConstraints;
  NbCon         = NbCon1 + 1;
  Dimension	= Pol1->Dimension+2;	/* homogeneous Dimension + Status */
  NbEle1	= NbCon1*Dimension;

  Mat = Matrix_Alloc(NbCon, Dimension);

  /* Set the constraints of Pol1 */
  Vector_Copy(Pol1->Constraint[0], Mat->p_Init, NbEle1);

  /* Add the new constraint */
  j = NbCon1;
  Mat->p[j][0] = 1;
  if (!(Pass&1)) for(i=1; i<Dimension; i++) Mat->p[j][i] = -Con2[i];
  else           for(i=1; i<Dimension; i++) Mat->p[j][i] =  Con2[i];
  if (!(Pass&2)) (Mat->p[j][Dimension-1])--;

  Ray = Matrix_Alloc(NbMaxRays, Dimension);
  Ray->NbRows = NbRay;
  Vector_Copy(Pol1->Ray[0], Ray->p_Init, NbRay*Dimension);

  Sat = BuildSat(Mat, Ray, NbCon1, NbMaxRays);
  Pol_status =
  Chernikova(Mat, Ray, Sat, Pol1->NbBid, NbMaxRays, NbCon1);
  Pol = Remove_Redundants(Mat, Ray, Sat, 0);
  Matrix_Free(Sat);
  Matrix_Free(Ray);
  Matrix_Free(Mat);
  return Pol;
} /* SubConstraint */

static Polyhedron *AddRays(Ray2, NbRay2, Pol1, NbMaxRays)
int        *Ray2;
unsigned   NbRay2;
Polyhedron *Pol1;
unsigned   NbMaxRays;
{
  Polyhedron *Pol;
  Matrix   *Mat, *Ray, *Sat, *Sat2;
  unsigned NbCon, NbRay, NbRay1, NbEle1, Dimension;

  NbCon		= Pol1->NbConstraints;
  NbRay1	= Pol1->NbRays;
  NbRay		= NbRay1 + NbRay2;
  Dimension     = Pol1->Dimension+2;	/* homogeneous Dimension + Status */
  NbEle1	= NbRay1*Dimension;

  Ray = Matrix_Alloc(NbRay, Dimension);

  /* Set the Rays of Pol1 */
  Vector_Copy(Pol1->Ray[0], Ray->p_Init, NbEle1);

  /* Add the new Rays */
  Vector_Copy(Ray2, Ray->p_Init+NbEle1, NbRay2*Dimension);

  Mat = Matrix_Alloc(NbMaxRays, Dimension);
  Mat->NbRows = NbCon;
  Vector_Copy(Pol1->Constraint[0], Mat->p_Init, NbCon*Dimension);

  Sat = BuildSat(Ray, Mat, NbRay1, NbMaxRays);
  Pol_status =
  Chernikova(Ray, Mat, Sat, Pol1->NbEq, NbMaxRays, NbRay1);
  Sat2 = TransformSat(Mat, Ray, Sat);
  Matrix_Free(Sat);
  Pol = Remove_Redundants(Mat, Ray, Sat2, 0);

  Matrix_Free(Sat2);
  Matrix_Free(Mat);
  Matrix_Free(Ray);
  return Pol;
} /* AddRays */

/* PolyhedronIncludes --
      Return 1 if Pol1 includes (covers) Pol2, 0 otherwise
*/
int	oldPolyhedronIncludes(Pol1, Pol2)
     Polyhedron *Pol1, *Pol2;
{ int Dimension = Pol1->Dimension + 1;
  int *p1, *p2, p3, i, j, k;
 
  for (k=0; k<Pol1->NbConstraints; k++)
  { for (i=0; i<Pol2->NbRays; i++)
    {   p1 = Pol2->Ray[i]+1;
        p2 = Pol1->Constraint[k]+1;
        p3 = 0;
        p3 = *p1++ * *p2++;
        for (j=1; j<Dimension; j++) p3 += *p1++ * *p2++;
	if ((p3<0) || (p3 && !Pol1->Constraint[k][0])) return 0;
    }
  }
  return 1;
}

int     PolyhedronIncludes(Pol1, Pol2)
     Polyhedron *Pol1, *Pol2;
{ int Dimension = Pol1->Dimension + 1;
  int *p1, *p2, p3, i, j, k;

  for (k=0; k<Pol1->NbConstraints; k++)
  { for (i=0; i<Pol2->NbRays; i++)
    {   p1 = Pol2->Ray[i]+1;
        p2 = Pol1->Constraint[k]+1;
        p3 = 0;
        p3 = *p1++ * *p2++;
        for (j=1; j<Dimension; j++) p3 += *p1++ * *p2++;
        if ((p3<0) || (p3 && !Pol1->Constraint[k][0])
                   || (p3 && !Pol2->Ray[i][0]       ) ) return 0;
    }
  }
  return 1;
}

/* AddPolyToDomain --
   add polyhedron p3 to domain p3beg, check for redundancies.
   if p3 is not used, it is removed from memory.
   return the new domain
*/
Polyhedron *AddPolyToDomain (p3, p3beg)
Polyhedron *p3, *p3beg;
{   Polyhedron *p, *p3end = (Polyhedron *) 0;
    int Redundant;

    if (!p3) return p3beg;
    if (!p3beg)	return p3;

    /* check for empty p3's */
    if ( emptyQ(p3) )
    {	Polyhedron_Free(p3);
	return p3beg;
    }

    /* test p3 against the domain */
    Redundant = 0;
    for (p=p3beg, p3beg=(Polyhedron *)0; p; p=p->next)
    {   if ( PolyhedronIncludes(p3, p) ) continue;

	/* add old polyhedron p to the new domain list */
	if (!p3beg) p3beg = p; else p3end->next = p;
	p3end = p;

	if ( PolyhedronIncludes(p, p3) ) /* p covers p3 */
	{   Redundant = 1;
	    Polyhedron_Free(p3);
	    break;
	}
    }

    if (!Redundant)	/* the whole list has been checked */
    {   /* add new polyhedron p3 to the new domain list */
	if (!p3beg) p3beg = p3; else p3end->next = p3;
    }
    else
    {   /* the rest of the list is just inherited from p */
	Polyhedron_Free(p3);
    }
    return p3beg;
}

Polyhedron *DomainIntersection(Pol1, Pol2, NbMaxRays)
     Polyhedron *Pol1, *Pol2;
     unsigned   NbMaxRays;
{   Polyhedron *p1, *p2, *p3, *d;

    if (!Pol1 || !Pol2) return (Polyhedron*) 0;
    if (Pol1->Dimension != Pol2->Dimension)
    {
#ifndef NO_MESSAGES
   fprintf(stderr,"? DomainIntersection: operation on different dimensions");
#endif
        Pol_status = 1;
        return (Polyhedron*) 0;
    }

    d = (Polyhedron *)0;
    for (p1=Pol1; p1; p1=p1->next)
    {	for (p2=Pol2; p2; p2=p2->next)
	{   p3 = AddConstraints(p2->Constraint[0],
				p2->NbConstraints, p1, NbMaxRays);
	    d = AddPolyToDomain (p3, d);
	}
    }
    if (!d) return Empty_Polyhedron(Pol1->Dimension); else return d;
} /* PolyhedronIntersection */

Polyhedron *DomainUnion(Pol1, Pol2, NbMaxRays)
     Polyhedron *Pol1, *Pol2;
     unsigned   NbMaxRays;
{ Polyhedron *PolA, *PolEndA, *PolB, *PolEndB, *p1, *p2;
  int Redundant;

    if (!Pol1 || !Pol2) return (Polyhedron*) 0;
    if (Pol1->Dimension != Pol2->Dimension)
    {
#ifndef NO_MESSAGES
        fprintf(stderr,"? DomainUnion: operation on different dimensions");
#endif
        Pol_status = 1;
        return (Polyhedron*) 0;
    }

  /* copy Pol1 to PolA */
  PolA = PolEndA = (Polyhedron *)0;
  for (p1=Pol1; p1; p1=p1->next)
  {  /* does any component of Pol2 cover it ? */
     Redundant = 0;
     for (p2=Pol2; p2; p2=p2->next)
     {  if ( PolyhedronIncludes(p2, p1) ) /* p2 covers p1 */
        {   Redundant = 1;
	    break;
	}
     }
     if (!Redundant)
     {   /* add p1 to Pol */
	if (!PolEndA)
	    PolEndA = PolA = Polyhedron_Copy(p1);
  	else
	{   PolEndA->next = Polyhedron_Copy(p1);
	    PolEndA = PolEndA->next;
	}
     }
  }

  /* copy Pol2 to PolB */
  PolB = PolEndB = (Polyhedron *)0;
  for (p2=Pol2; p2; p2=p2->next)
  {  /* does any component of Pol cover it ? */
     Redundant = 0;
     for (p1=PolA; p1; p1=p1->next)
     {  if ( PolyhedronIncludes(p1, p2) ) /* p1 covers p2 */
        {   Redundant = 1;
	    break;
	}
     }
     if (!Redundant)
     {   /* add p2 to PolB */
	if (!PolEndB)
	    PolEndB = PolB = Polyhedron_Copy(p2);
  	else
	{   PolEndB->next = Polyhedron_Copy(p2);
	    PolEndB = PolEndB->next;
	}
     }
  }

  if (!PolA) return PolB;
  PolEndA->next = PolB;
  return PolA;
} /* DomainUnion */

Polyhedron *DomainConvex(Pol1, NbMaxRays)
     Polyhedron *Pol1;
     int NbMaxRays;
{   Polyhedron *p, *Pol, *PolNew;

    if (!Pol1) return (Polyhedron*) 0;

    Pol = Polyhedron_Copy(Pol1);
    for (p=Pol1->next; p; p=p->next)
    {	PolNew = AddRays(p->Ray[0], p->NbRays, Pol, NbMaxRays);
	Polyhedron_Free(Pol);
	Pol = PolNew;
    }
    return Pol;
} /* DomainConvex */

Polyhedron *DomainDifference(Pol1, Pol2, NbMaxRays)
     Polyhedron *Pol1, *Pol2;
     unsigned NbMaxRays;
{   Polyhedron *p1, *p2, *p3, *d;
    int i;

    if (!Pol1 || !Pol2) return (Polyhedron*) 0;
    if (Pol1->Dimension != Pol2->Dimension)
    {
#ifndef NO_MESSAGES
        fprintf(stderr,"? DomainDifference: operation on different dimensions");
#endif
        Pol_status = 1;
        return (Polyhedron*) 0;
    }

    d = (Polyhedron *)0;
    for (p2=Pol2; p2; p2=p2->next)
    {	for (p1=Pol1; p1; p1=p1->next)
	{   for (i=0; i<p2->NbConstraints; i++)
	    {	p3 = SubConstraint(p2->Constraint[i], p1, NbMaxRays,0);
		d = AddPolyToDomain (p3, d);
		if (p2->Constraint[i][0]!=0) continue;  /*inequality*/
		p3 = SubConstraint(p2->Constraint[i], p1, NbMaxRays,1);
                d = AddPolyToDomain (p3, d);
	    }
	}
	Pol1 = d;
	d = (Polyhedron *)0;
    }
    if (!Pol1) return Empty_Polyhedron(Pol2->Dimension); else return Pol1;
} /* DomainDifference */

Polyhedron *Polyhedron_Preimage(Pol1, Func, NbMaxRays)
     Polyhedron *Pol1;
     Matrix     *Func;
     unsigned   NbMaxRays;
{
  Matrix *Constraints;
  Polyhedron *Pol2;
  unsigned NbConstraints, Dimension1, Dimension2;
  int i, j, k, Sum;

  NbConstraints = Pol1->NbConstraints;
  Dimension1    = Pol1->Dimension+1;	/* homogeneous Dimension */
  Dimension2    = Func->NbColumns;	/* homogeneous Dimension */
  if (Dimension1!=(Func->NbRows))
  {
#ifndef NO_MESSAGES
      fprintf(stderr,"? Preimage: incompatable dimensions\n");
#endif
      Pol_status = 1;
      return Empty_Polyhedron(Dimension2-1);
  }
/*      Dim1           Dim2            Dim2
	__k__          __j__           __j__	
  NbCon |   |  X   Dim1|   |  =  NbCon |   |
    i   |___|       k  |___|       i   |___|
     Pol1->Constraints Function        Constraints
*/
  Constraints = Matrix_Alloc(NbConstraints, Dimension2+1);
  for (i=0; i<NbConstraints; i++) {
    Constraints->p[i][0] = Pol1->Constraint[i][0];
    for (j=0; j<Dimension2; j++) {
      Sum = 0;
      for (k=0; k<Dimension1; k++)
        Sum+=Pol1->Constraint[i][k+1] * Func->p[k][j];
      		/* k = Dimension1-1 :
		Perform as though last row of Func where [ 0 0 ... 0 1 ] */
		/*if(j==(Dimension2-1))Sum+=Pol1->Constraint[i][Dimension1];*/
      Constraints->p[i][j+1] = Sum;
    }
  }
  Pol2 = Constraints2Polyhedron(Constraints, NbMaxRays);
  Matrix_Free(Constraints);
  return Pol2;
} /* Polyhedron_Preimage */

Polyhedron *DomainPreimage(Pol1, Func, NbMaxRays)
     Polyhedron *Pol1;
     Matrix     *Func;
     unsigned   NbMaxRays;
{
    Polyhedron *p, *p1, *d;

    if (!Pol1 || !Func) return (Polyhedron *) 0;
    d = (Polyhedron *) 0;
    for (p=Pol1; p; p=p->next)
    {	p1 = Polyhedron_Preimage(p, Func, NbMaxRays);
	d = AddPolyToDomain (p1, d);
    } 
    return d;
}

Polyhedron *Polyhedron_Image(Pol1, Func, NbMaxRays)
     Polyhedron *Pol1;
     Matrix     *Func;
     unsigned   NbMaxRays;
{
  Matrix *Rays;
  Polyhedron *Pol2;
  unsigned NbRays, Dimension1, Dimension2;
  int i, j, k, Sum;

  NbRays     = Pol1->NbRays;
  Dimension1 = Pol1->Dimension+1;	/* homogeneous Dimension */
  Dimension2 = Func->NbRows;		/* homogeneous Dimension */
  if (Dimension1!=Func->NbColumns)
  {
#ifndef NO_MESSAGES
      fprintf(stderr,"? Image: incompatable dimensions\n");
#endif
      Pol_status = 1;
      return Empty_Polyhedron(Dimension2-1);
  }
/*      Dim1     /      Dim1  \Transpose      Dim2
        __k__    |      __k__ |              __j__
  NbRays|   |  X | Dim2 |   | |     =  NbRays|   |
    i   |___|    |   j  |___| |          i   |___|
     Pol1->Rays  \       Func /               Rays
*/
  Rays = Matrix_Alloc(NbRays, Dimension2+1);
  for (i=0; i<NbRays; i++) {
    Rays->p[i][0] = Pol1->Ray[i][0];
    for (j=0; j<Dimension2; j++) {
      Sum = 0;
      for (k=0; k<Dimension1; k++)
        Sum+=Pol1->Ray[i][k+1] * Func->p[j][k];
      Rays->p[i][j+1] = Sum;
    }
	/* j = Dimension2 :
	Perform as though last row of Func where [ 0 0 ... 0 1 ] */
	/* Rays->p[i][Dimension2+1]=Pol1->Ray[i][Dimension1];*/
  }
  Pol2 = Rays2Polyhedron(Rays, NbMaxRays);
  Matrix_Free(Rays);
  return Pol2;
} /* Polyhedron_Image */

Polyhedron *DomainImage(Pol1, Func, NbMaxRays)
     Polyhedron *Pol1;
     Matrix     *Func;
     unsigned   NbMaxRays;
{
    Polyhedron *p, *p1, *d;

    if (!Pol1 || !Func) return (Polyhedron *) 0;
    d = (Polyhedron *) 0;
    for (p=Pol1; p; p=p->next)
    {   p1 = Polyhedron_Image(p, Func, NbMaxRays);
        d = AddPolyToDomain (p1, d);
    }
    return d;
}

Polyhedron *DomainSimplify(Pol1, Pol2, NbMaxRays)
     Polyhedron *Pol1, *Pol2;
     unsigned   NbMaxRays;
{   Polyhedron *p1, *p2, *p3, *d;
    unsigned k, jx, bx, nc, NbConstraints, RowSize, Dimension, *Filter, NbCon2;
    unsigned vid;
    Matrix *Constraints;

    if (!Pol1 || !Pol2) return Pol1;
    if (Pol1->Dimension != Pol2->Dimension)
    {
#ifndef NO_MESSAGES
        fprintf(stderr,"? DomainSimplify: operation on different dimensions");
#endif
        Pol_status = 1;
        return 0;
    }
    if (emptyQ(Pol1)||emptyQ(Pol2)) return Empty_Polyhedron(Pol1->Dimension);

    NbCon2 = 0;
    for (p2=Pol2; p2; p2=p2->next)
        if (p2->NbConstraints > NbCon2) NbCon2 = p2->NbConstraints;

    Dimension = Pol1->Dimension+2;    /* allocation Dimension */
    d = (Polyhedron *)0;
    for (p1=Pol1; p1; p1=p1->next)
    { 
      NbConstraints = p1->NbConstraints;
      nc = (NbConstraints + NbCon2 - 1) / (sizeof(int)*8) + 1;
      RowSize = nc*sizeof(int);
      Filter  = (unsigned *)malloc(RowSize);
      Vector_Init(Filter, nc);

      /* Filter the constraints of p1 in context of p2(s) */
      vid = 1;
      for (p2=Pol2; p2; p2=p2->next)
      {   if (SimplifyConstraints(p1, p2, Filter, NbMaxRays)) vid=0;
          /* takes the union of all non redundant constraints */
      }

      if (!vid)
      {  /* copy all non-redundant constraints to matrix */
         Constraints = Matrix_Alloc(NbConstraints, Dimension);
         Constraints->NbRows = 0;
         for (k=0, jx=0, bx=MSB; k<NbConstraints; k++)
         {  if (Filter[jx]&bx) /* copy constraint */
            {  Vector_Copy(p1->Constraint[k],
                           Constraints->p[Constraints->NbRows],
                           Dimension);
               Constraints->NbRows++;
            }
            NEXT(jx,bx);
         }
         p3 = Constraints2Polyhedron(Constraints,NbMaxRays);
         Matrix_Free(Constraints);
         d = AddPolyToDomain (p3, d);
      }
      free(Filter);
    }
    if (!d) return Empty_Polyhedron(Pol1->Dimension); else return d;
} /* DomainSimplify */

Polyhedron *DomainAddRays(Pol1, Mat2, NbMaxRays)
     Polyhedron *Pol1;
     Matrix *Mat2;
     unsigned   NbMaxRays;
{ Polyhedron *PolA, *PolEndA, *p1, *p2, *p3;
  int Redundant;

    if (!Pol1 ) return (Polyhedron*) 0;
    if (!Mat2 ) return Pol1;
    if (Pol1->Dimension != Mat2->NbColumns-2)
    {
#ifndef NO_MESSAGES
        fprintf(stderr,"? DomainAddRays: operation on different dimensions");
#endif
        Pol_status = 1;
        return (Polyhedron*) 0;
    }

  /* copy Pol1 to PolA */
  PolA = PolEndA = (Polyhedron *)0;
  for (p1=Pol1; p1; p1=p1->next)
  {  p3 = AddRays(Mat2->p_Init, Mat2->NbRows, p1, NbMaxRays);
     /* does any component of PolA cover it ? */
     Redundant = 0;
     for (p2=PolA; p2; p2=p2->next)
     {  if ( PolyhedronIncludes(p2, p1) ) /* p2 covers p1 */
        {   Redundant = 1;
	    break;
	}
     }
     if (!Redundant)
     {   /* add p1 to PolA */
	if (!PolEndA)
	    PolEndA = PolA = p3;
  	else
	{   PolEndA->next = p3;
	    PolEndA = PolEndA->next;
	}
     }
  }
  return PolA;
} /* DomainAddRays */

Polyhedron *DomainAddConstraints(Pol1, Mat2, NbMaxRays)
     Polyhedron *Pol1;
     Matrix *Mat2;
     unsigned   NbMaxRays;
{ Polyhedron *PolA, *PolEndA, *p1, *p2, *p3;
  int Redundant;

    if (!Pol1 ) return (Polyhedron*) 0;
    if (!Mat2 ) return Pol1;
    if (Pol1->Dimension != Mat2->NbColumns-2)
    {
#ifndef NO_MESSAGES
 fprintf(stderr,"? DomainAddConstraints: operation on different dimensions\n");
#endif
        Pol_status = 1;
        return (Polyhedron*) 0;
    }

  /* copy Pol1 to PolA */
  PolA = PolEndA = (Polyhedron *)0;
  for (p1=Pol1; p1; p1=p1->next)
  {  p3 = AddConstraints(Mat2->p_Init, Mat2->NbRows, p1, NbMaxRays);
     /* does any component of PolA cover it ? */
     Redundant = 0;
     for (p2=PolA; p2; p2=p2->next)
     {  if ( PolyhedronIncludes(p2, p1) ) /* p2 covers p1 */
        {   Redundant = 1;
	    break;
	}
     }
     if (!Redundant)
     {   /* add p1 to PolA */
	if (!PolEndA)
	    PolEndA = PolA = p3;
  	else
	{   PolEndA->next = p3;
	    PolEndA = PolEndA->next;
	}
     }
  }
  return PolA;
} /* DomainAddRays */

