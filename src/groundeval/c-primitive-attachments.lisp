;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c-primitive-attachments.lisp -- creates C attachments for primitives defined
;;                                 in the prelude
;; Author          : Shankar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2019, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

;;(clrhash *c-primitive-attachments-hash*)
(def-c-attach-primitive "integertypes" "u8plus" 
  "uint8" '(x y) '(uint8 uint8) "{return (uint8_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u16plus" 
  "uint16" '(x y) '(uint16 uint16) "{return (uint16_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u32plus" 
  "uint32"  '(x y) '(uint32 uint32) "{return (uint32_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u64plus" 
  "uint64"  '(x y) '(uint64 uint64) "{return (uint64_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u8minus"
  "uint8" '(x y) '(uint8 uint8) "{return (uint8_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u16minus"
  "uint16" '(x y) '(uint16 uint16) "{return (uint16_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u32minus" 
  "uint32" '(x y) '(uint32 uint32) "{return (uint32_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u64minus"
  "uint64" '(x y) '(uint64 uint64) "{return (uint64_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u8times" 
  "uint8" '(x y) '(uint8 uint8) "{return (uint8_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u16times"
  "uint16" '(x y) '(uint16 uint16) "{return (uint16_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u32times" 
  "uint32" '(x y) '(uint32 uint32) "{return (uint32_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u64times" 
  "uint64" '(x y) '(uint64 uint64) "{return (uint64_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u8div" 
  "uint8"  '(x y) '(uint8 uint8) "{return (uint8_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u16div" 
  "uint16" '(x y) '(uint16 uint16) "{return (uint16_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u32dixv" 
  "uint32" '(x y)  '(uint32 uint32) "{return (uint32_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u64div" 
  "uint64" '(x y) '(uint64 uint64) "{return (uint64_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u8rem" 
  "uint8" '(x y)  '(uint8 uint8) "{return (uint8_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u16rem" 
  "uint16" '(x y) '(uint16 uint16) "{return (uint16_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u32rem"
  "uint32" '(x y) '(uint32 uint32) "{return (uint32_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u64rem"
  "uint64" '(x y) '(uint64 uint64) "{return (uint64_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u8pow2"
  "uint8"  '(x) '(uint8) "{return (uint8_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u16pow2" 
  "uint16" '(x) '(uint16) "{return (uint16_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u32pow2"
  "uint32" '(x) '(uint32) "{return (uint32_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u64pow2"
  "uint64" '(x) '(uint64) "{return (uint64_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u8lshift"
  "uint8" '(x n) '(uint8  uint8) "{return (uint8_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u16lshift"
  "uint16" '(x n) '(uint16 uint16) "{return (uint16_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u32lshift"
  "uint32" '(x n) '(uint32 uint32) "{return (uint32_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u64lshift"
  "uint64" '(x n) '(uint64 uint64) "{return (uint64_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u8rshift"
  "uint8"  '(x n) '(uint8 uint8) "{return (uint8_t)x>>n;}")

(def-c-attach-primitive "integertypes" "u16rshift" 
  "uint16" '(x n) '(uint16 uint16) "{return (uint16_t)x>>n;}")

(def-c-attach-primitive "integertypes" "u32rshift"
  "uint32" '(x n) '(uint32 uint32) "{return (uint32_t)x>>n;}")

(def-c-attach-primitive "integertypes" "u64rshift"
  "uint64" '(x n) '(uint64 uint64) "{return (uint64_t)x>>n;}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Floating point attachments using NASA's ieee754_double.pvs

(def-c-attach-primitive "ieee754_double" "pInf" "double" nil nil  "{return (double_t)INFINITY;}")

(def-c-attach-primitive "ieee754_double" "nInf" "double" nil nil  "{return (double_t)-INFINITY;}")

(def-c-attach-primitive "ieee754_double" "finite?" "bool" '(x) '(double)
  "{return (bool_t)isfinite(x);}")

(def-c-attach-primitive "ieee754_double" "NaN?" "bool" '(x) '(double)
  "{return (bool_t)isnan(x);}")

(def-c-attach-primitive "ieee754_double" "pZero" "double" nil nil
  "{return (double_t)0.0;}")

(def-c-attach-primitive "ieee754_double" "nZero" "double" nil nil
  "{return (double_t)-0.0;}")

(def-c-attach-primitive "ieee754_double" "DtoR" "mpq_ptr" '(x)  '(double)
  "{mpq_ptr_t result; result = safe_malloc(sizeof(mpq_t)); mpq_init(result); mpq_set_d(result, x); return result;}")

(def-c-attach-primitive "ieee754_double" "RtoD" "double" '(x) '(mpq)
  "{return mpq_get_d(x);}")

(def-c-attach-primitive "ieee754_double" "nzfinite?" "bool" '(x) '(double)
  "{return (isfinite(x) && x != 0.0 && x != -0.0);}")

(def-c-attach-primitive "ieee754_double" "pfinite?" "bool" '(x) '(double)
  "{return (isfinite(x) && x > 0.0);}")

(def-c-attach-primitive "ieee754_double" "nfinite?" "bool" '(x) '(double)
  "{return (isfinite(x) && x <= -0.0);}")

(def-c-attach-primitive "ieee754_double" "add" "double" '(x y) '(double double)
  "{return (double_t)(x + y);}")

(def-c-attach-primitive "ieee754_double" "sub" "double" '(x y) '(double double)
  "{return (double_t)(x - y);}")

(def-c-attach-primitive "ieee754_double" "mul" "double" '(x y) '(double double)
  "{return (double_t)(x * y);}")

(def-c-attach-primitive "ieee754_double" "div" "double" '(x y) '(double double)
  "{return (double_t)(x / y);}")

(def-c-attach-primitive "ieee754_double" "min" "double" '(x y) '(double double)
  "{return (double_t)fmin(x, y);}")

(def-c-attach-primitive "ieee754_double" "max" "double" '(x y) '(double double)
  "{return (double_t)fmax(x, y);}")

(def-c-attach-primitive "ieee754_double" "abs" "double" '(x y) '(double)
  "{return (double_t)fabs(x);}")

(def-c-attach-primitive "ieee754_double" "sqrt" "double" '(x y) '(double)
  "{return (double_t)sqrt(x);}")

(def-c-attach-primitive "ieee754_double" "to_double" "double" '(x) '(mpq)
  "{return mpq_get_d(x);}")

(def-c-attach-primitive "ieee754_double" "add_double" "double" '(x y) '(double double)
  "{return (double_t)(x + y);}")

(def-c-attach-primitive "ieee754_double" "sub_double" "double" '(x y) '(double double)
  "{return (double_t)(x - y);}")

(def-c-attach-primitive "ieee754_double" "mul_double" "double" '(x y) '(double double)
  "{return (double_t)(x * y);}")

(def-c-attach-primitive "ieee754_double" "div_double" "double" '(x y) '(double double)
  "{return (double_t)(x / y);}")

(def-c-attach-primitive "ieee754_double" "min_double" "double" '(x y) '(double double)
  "{return (double_t)fmin(x, y);}")

(def-c-attach-primitive "ieee754_double" "max_double" "double" '(x y) '(double double)
  "{return (double_t)fmax(x, y);}")

(def-c-attach-primitive "ieee754_double" "abs_double" "double" '(x) '(double)
  "{return (double_t)fabs(x);}")

(def-c-attach-primitive "ieee754_double" "sqrt_double" "double" '(x) '(double)
  "{return (double_t)sqrt(x);}")

(def-c-attach-primitive "ieee754_double" "le_double" "bool" '(x y) '(double double)
  "{return (bool_t)(x <= y);}")

(def-c-attach-primitive "ieee754_double" "ge_double" "bool" '(x y) '(double double)
  "{return (bool_t)(x >= y);}")

(def-c-attach-primitive "ieee754_double" "lt_double" "bool" '(x y) '(double double)
  "{return (bool_t)(x < y);}")

(def-c-attach-primitive "ieee754_double" "gt_double" "bool" '(x y) '(double double)
  "{return (bool_t)(x > y);}")

(def-c-attach-primitive "ieee754_double" "eq_double" "bool" '(x y) '(double double)
  "{return (bool_t)(x == y);}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Experimental native treatment of strings using a cstring.pvs theory
(def-c-attach-primitive-type "string" "cchar" "char")

(def-c-attach-primitive-type "cstring" "cstring" "string_t")

(def-c-attach-primitive "cstring" "mk_cstring" "cstring" '(s) '(strings_string)
 (format nil "{~%~8Tuint16_t length = (uint16_t)mpz_get_ui(s->length);~
~%~8T cstring_cstring_t result = (cstring_cstring_t)safe_malloc(sizeof(string_s) + sizeof(length) + 1);~
~%~8T result->count = 1; result->size = length + 1;~
~%~8T uint16_t i;~
~%~8T for (i = 0; i < length; i++){result->strval[i] = s->seq[i];};~
~%~8T result->strval[i] = '\\0'; return result;}"))

(def-c-attach-primitive "cstring" "code" "uint8" '(x) '(cstring_cchar)
  "{return (uint8_t)x;}")

(def-c-attach-primitive "cstring" "empty" "cstring_cstring" '() '()
  (format nil "{~%~8Tcstring_cstring_t result = (cstring_cstring_t)safe_malloc(sizeof(string_s) + sizeof(char));~
 ~%~8T result->count = 1; result->size = 1; result->strval[0] = '\\0';~
  ~%~8T return result;}"))

(def-c-attach-primitive "cstring" "char" "cstring_cchar" '(s i) '(cstring_cstring uint16)
  (format nil "{if (i < strlen(s)){char c = s->strval[i];~%~8T if (s->count > 1){s->count--;} else {safe_free(s);}~
~%~8T return c;}~
~%~4T else return '\0';}"))

(def-c-attach-primitive "cstring" "length" "uint16" '(s) '(cstring_cstring)
  (format nil "{uint16_t length = (uint16_t)strlen(s->strval);~
~%~8T if (s->count > 1){s->count--;} else {safe_free(s);}~
~%~8T return length;}"))

(def-c-attach-primitive "cstring" "+" "cstring_cstring" '(s1 s2) '(cstring_cstring cstring_cstring)
  (format nil "{~%~8T uint16_t l1 = strlen(s1->strval);~
~%~8T uint16_t l2 = strlen(s1->strval);~
~%~8T if (l1  == 0){return s2;} else {if (l2 == 0) return s2;}
~%~8T uint16_t length = l1 + l2;~
~%~8T uint16_t size = length + 1;~
~%~8T result = (cstring_cstring_t)safe_malloc(sizeof(string_s) + size);~
~%~8T result->count = 1;~
~%~8T result->size = size;~
~%~8T strcpy(result->strval, s1->strval);~
~%~8T strcat(result->strval, s2->strval);~
~%~8Tif (s1->count > 1){s1->count--;} else {safe_free(s1)};~
~%~8Tif (s2->count > 1){s2->count--;} else {safe_free(s2)};~
~%~8Treturn result;~%}"))

(def-c-attach-primitive "cstring" "charcmp" "bool" '(c1 c2) '(cstring_cchar cstring_cchar)
  "{return c1 < c2;}")

(def-c-attach-primitive "cstring" "strdiff" "uint16" '(s1 s2) '(cstring_cstring cstring_cstring)
  (format nil "{uint16_t i = 0;~%~8Twhile ((i < strlen(s1->strval)) && (i < strlen(s2->strval)) && (s1->strval[i] == s2->strval[i])){i++;};~
~%~8Tif (s1->count > 1){s1->count--;} else {safe_free(s1);}~
~%~8Tif (s2->count > 1){s1->count--;} else {safe_free(s2);}~
~%~8T return i;}"))
  
(def-c-attach-primitive "cstring" "strcmp" "int8" '(s1 s2) '(cstring_cstring cstring_cstring)
  (format nil "{~%~8T int8 cmp = strcmp(s1->strval, s2->strval);~
~%~8Tif (s1->count > 1){s1->count--;} else {safe_free(s1);}~
~%~8Tif (s2->count > 1){s2->count--;} else {safe_free(s2);}~
~%~8Treturn (cmp < 0)? -1 : (cmp > 0) ?  1 : 0;~%}"))

(def-c-attach-primitive "cstring" "substr" "cstring_cstring" '(s i j) '(cstring_cstring uint16 uint16)
  (format nil "{uint16_t length = (i > j) ? i - j : j - i;~
~%~8T uint16_t start = (i > j) ?  j :  i;~
~%~8T cstring_cstring_t result = (cstring_cstring_t)safe_malloc(sizeof(string_s) + length);~
~%~8T result->count = 1; result->size = length;~
~%~8T memcpy(result->strval, (char *)(s + start), length);
~%~8Tif (s->count > 1){s->count--;} else {safe_free(s);}~
~%~8T return result; }"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C attachments for pvsio_prelude theories stdstr and stdio

(def-c-attach-primitive "stdstr" "charcode" "string" '(x) '(uint8)
  "{char c = (char) x;~%~8Tstring_t result = (string_t)safe_malloc(sizeof(string_s) + 2 * sizeof(char));~
 ~%~8T result->count = 1; result->size = 2; result->strval[0] = c; ~%~8T result->strval[1] = '\0';~
  ~%~8T return result;}")


(def-c-attach-primitive "stdstr" "emptystr" "string" '() '()
  "{~%~8Tstring_t result = (string_t)safe_malloc(sizeof(string_s) + sizeof(char));~
 ~%~8T result->count = 1; result->size = 1; result->strval[0] = '\0';~
  ~%~8T return result;}")

(def-c-attach-primitive "stdstr" "space" "string" '() '()
  "{return stdstr_charcode(32);}")

(def-c-attach-primitive "stdstr" "newline" "string" '() '()
  "{return stdstr_charcode(10);}")

(def-c-attach-primitive "stdstr" "tab" "string" '() '()
  "{~%~8Tstring_t result = (string_t)safe_malloc(sizeof(string_s) + 9 * sizeof(char));~
 ~%~8T result->count = 1; result->size = 9; result->strval = \"        \";~
  ~%~8T return result;}")

(def-c-attach-primitive "stdstr" "doublequote" "string" '() '()
  "{return stdstr_charcode(34);}")

(def-c-attach-primitive "stdstr" "singlequote" "string" '() '()
  "{return stdstr_charcode(39);}")

(def-c-attach-primitive "stdstr" "backquote" "string" '() '()
  "{return stdstr_charcode(96);}")

(def-c-attach-primitive "stdstr" "spaces" "string" '(n) '(uint16)
   "{~%~8Tuint16_t size = n * sizeof(char) + sizeof(char)~
 ~%~8Tstring_t result = (string_t)safe_malloc(sizeof(string_s) + size);~
 ~%~8T result->count = 1;
 ~%~8T result->size = size;
 ~%~8T for (uint32 i = 0; i < n; i++){result->strval[i] = ' ';}
 ~%~8T result->strval[n] = '\0';
  ~%~8T return result;}")

(def-c-attach-primitive "stdstr" "strlen" "string" '(s) '(uint16)
  "{~%~8T if (s->count = 1){
~%~16T uint16_t ret = strlen(s->strval); safe_free(s); return ret;}
~%~8T else {s->count--; return strlen(s->strval);}
~%~8T}")

(def-c-attach-primitive "stdstr" "upcase" "string" '(s) '(string)
   "{~%~8Tstring_t result = s; //init to s
     if (s->count > 1){~
 ~%~16T uint16_t length = strlen(s->strval);
 ~%16T uint16_t size = length * sizeof(char) + sizeof(char);~
 ~%~16T result = (string_t)safe_malloc(sizeof(string_s) + size);~
~%~16T result->size = size;~
~%~16T result->count = 1;~
~%~16T s->count--;}~ 
~%~8T}~ 
~%~8T for (uint32 i = 0; i < length; i++){char c = s->strval[i];~
~%~24T if (c > 96 && c < 123){s->strval[i] = c - 32;}~
  ~%~8T return result;}")

(def-c-attach-primitive "stdstr" "downcase" "string" '(s) '(string)
   "{~%~8Tstring_t result = s; //init to s
     if (s->count > 1){~
 ~%~16T uint32_t length = strlen(s->strval);
 ~%16T uint32_t size = length * sizeof(char) + sizeof(char);~
 ~%~16T result = (string_t)safe_malloc(sizeof(string_s) + size);~
~%~16T result->size = size;~
~%~16T result->count = 1;~
~%~16T s->count--;}~ 
~%~8T}~ 
~%~8T for (uint32 i = 0; i < length; i++){char c = s->strval[i];~
~%~24T if (c > 64 && c < 91){s->strval[i] = c + 32;}~
  ~%~8T return result;}")

(def-c-attach-primitive "stdstr" "strfind" "uint32_t" '(s1 s2) '(string string)
   "{~%~8Tchar* result = strstr(s2->strval, s1->strval);~
~%~8Tuint32_t ret;
~%~8Tif result = NULL {ret = -1;}~
~%~8Telse {ret = (result - s2->strval);}
~%~8Tif (s1->count == 1){safe_free(s1);}
~%~8Tif (s2->count == 1){safe_free(s2);}
~%~8Treturn ret;~%}")

(def-c-attach-primitive "stdstr" "concat" "string" '(s1 s2) '(string string)
   "{~%~8T uint32_t l1 = strlen(s1->strval);~
~%~8T uint32_t l2 = strlen(s1->strval);~
~%~8T uint32_t length = l1 + l2;~
~%~8T uint32_t size = length + 1;~
~%~8T result = (string_t)safe_malloc(sizeof(string_s) + size);~
~%~8T result->count = 1;~
~%~8T result->size = size;~
~%~8T strcpy(result->strval, s1->strval);~
~%~8T strcat(result->strval, s2->strval);~
~%~8Tif (s1->count == 1){safe_free(s1)};~
~%~8Tif (s2->count == 1){safe_free(s2)};~
~%~8Treturn result;~%}")

(def-c-attach-primitive "stdstr" "strcmp" "uint8" '(s1 s2) '(string string)
  "{~%~8T uint8_t result = strcmp(s1->strval, s2->strval);~
~%~8Tif (s1->count == 1){safe_free(s1)};~
~%~8Tif (s2->count == 1){safe_free(s2)};~
~%~8Treturn result;~%}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;stdio primitives

(def-c-attach-primitive "stdstr" "strcmp" "uint8" '(s1 s2) '(string string)
  "{~%~8T uint8_t result = strcmp(s1->strval, s2->strval);~
~%~8Tif (s1->count == 1){safe_free(s1)};~
~%~8Tif (s2->count == 1){safe_free(s2)};~
~%~8Treturn result;~%}")



















