#include <stdio.h>
#include "stdansi.h"
#include "types.h"
#include "vector.h"
#include "polyhedron.h"

void My_Matrix_Print(Mat)
     Matrix * Mat;
{
  
  Matrix_Print(stdout,"%d ", Mat);
  fflush(stdout);
} /* My_Matrix_Print */


void My_Polyhedron_Print(Poly)
      Polyhedron* Poly;
{
  
  Polyhedron_Print(stdout,"%d ", Poly);
  fflush(stdout);
} /* My_Polyhedron_Print */

void Domain_Print_rec(Dst, Format, Pol)
     FILE *Dst;
     char *Format;
     Polyhedron *Pol;
{
  Polyhedron_Print(Dst, Format, Pol);
  if (Pol->next)
    { fprintf(Dst, ",");
      Domain_Print_rec(Dst, Format, Pol->next);
    };
} /* Domain_Print_rec */

void Domain_Print(Dst, Format, Pol)
     FILE *Dst;
     char *Format;
     Polyhedron *Pol;
{
  fprintf(Dst, "DOMAIN: {\n");
  Domain_Print_rec(Dst, Format, Pol);
  fprintf(Dst, "}");
} /* Domain_Print */

void My_Domain_Print(Poly)
     Polyhedron* Poly;
{
  
  Domain_Print(stdout,"%d ", Poly);
  fflush(stdout);
} /* My_Domain_Print */

unsigned inconsistent_p(P)
     Polyhedron* P;
{
  return P->NbRays==0;
} /* inconsistent_p */

unsigned universe_p(P)
     Polyhedron* P;
{
  return P->Dimension==P->NbBid;
} /* universe_p */

int Pol_status_f()
{
  return Pol_status;
} /* Pol_status_f */

Matrix *Matrix_Set(Mat, row, col, val)
     Matrix *Mat;
     unsigned row, col;
     int val;
{
  Mat->p[row][col] = val;
  return Mat;
} /* Matrix_Set */

int Matrix_Ref(Mat, row, col)
     Matrix *Mat;
     unsigned row, col;
{
  return Mat->p[row][col];
} /* Matrix_Ref */

/*
int strict_inconsistent_p(P)
Polyhedron *P;
{

   A polyehedron is empty if it includes (epsilon <= 0)
  return PolyhedronIncludes(EPSLEQ0,P);

}
*/

/* MyPolyhedronIncludes --
      Return 1 if Pol1 includes (covers) Pol2, 0 otherwise
*/
int	MyPolyhedronIncludes(Pol1, Pol2)
     Polyhedron *Pol1, *Pol2;
{ int Dimension = Pol1->Dimension + 1;
  int *p1, *p2, p3, i, j, k;
  /* fprintf(stdout, "polyincludes: {\n");
     fflush(stdout);
     */
  for (k=0; k<Pol1->NbConstraints; k++)
  { for (i=0; i<(Pol2->NbRays); i++)
    {   p1 = Pol2->Ray[i]+1;
        p2 = Pol1->Constraint[k]+1;
        p3 = 0;
        p3 = *p1++ * *p2++;
        for (j=1; j<Dimension; j++) p3 += *p1++ * *p2++;
	if ((p3<0) || (p3 && (!(Pol1->Constraint[k][0]) || 
			      !(Pol2->Ray[i][0])))) return 0;
    }
  }
  return 1;
}


/* DomainIncludesPolyhedron --
      Return 1 if Exists a Pol1 in Dom1 that includes (covers) Pol2,
      0 otherwise
*/
int	DomainIncludesPolyhedron(Dom1, Pol2)
     Polyhedron *Dom1, *Pol2;
{ int pol1_includes;
  if (!Dom1) return 0;
  pol1_includes = MyPolyhedronIncludes(Dom1, Pol2);
  if (pol1_includes) return 1;
  if (Dom1->next) return DomainIncludesPolyhedron(Dom1->next, Pol2);
     else return 0;
}

/* MyDomainIncludes --
      Return 1 if Dom1 includes (covers) Dom2, 0 otherwise
      This is approximated as Forall Pol2 in Dom2 there exists
      a Pol1 in Dom1 such that Pol1 includes Pol2.
*/
int	MyDomainIncludes(Dom1, Dom2)
     Polyhedron *Dom1, *Dom2;
{ int pol2_included;
  pol2_included = DomainIncludesPolyhedron(Dom1, Dom2);
  if (!pol2_included) return 0;
  if (Dom2->next) return MyDomainIncludes(Dom1, Dom2->next);
     else return 1;
}

Polyhedron *OldMyDomainAddConstraints(Pol1, Mat2, NbMaxRays)
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
     {  if ( PolyhedronIncludes(p2, p3) ) /* p2 covers p3 */
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
} /* OldMyDomainAddConstraints */

Polyhedron *MyDomainAddConstraints(Pol1, Mat2, NbMaxRays)
     Polyhedron *Pol1;
     Matrix *Mat2;
     unsigned   NbMaxRays;
{ Polyhedron *PolA, *PolEndA, *p1, *p2, *p3, *p2A, *p2Next;
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
    /* does any component of PolA cover it or is covered by it? */
    Redundant = 0;
    p2A = (Polyhedron *)0;
    p2 = PolA;
    while (p2) /* for(p2=PolA; p2; p2=p2Next) */
      {
      if ( PolyhedronIncludes(p3, p2) ) /* p3 covers p2 */
	{
	{if (!p2A)
	  PolA = p2->next;
	else
	  p2A->next = p2->next;
	if (PolEndA = p2) PolEndA = p2A;
	p2Next = p2->next;
	Polyhedron_Free(p2);
	p2 = p2Next;
	}}
      else
	{
	if ( PolyhedronIncludes(p2, p3) ) /* p2 covers p3 */
	  {   Redundant = 1;
	  Polyhedron_Free(p3);
	  break;
	  }
	else p2 = p2->next;
	}
      }
    if (!Redundant)
      {   /* add p3 to PolA */
	if (!PolEndA)
	  {
	  PolEndA = PolA = p3;}
  	else
	  { /* My_Polyhedron_Print(PolEndA); */
	    PolEndA->next = p3;
	    PolEndA = PolEndA->next;
	  }
      }
    }
  return PolA;
} /* MyDomainAddConstraints */

