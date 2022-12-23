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

(def-c-attach-primitive "integertypes" "u8max"
  "uint8" '(x y) '(uint8 uint8) "{return (uint8_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "u16max"
  "uint16" '(x y) '(uint16 uint16) "{return (uint16_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "u32max"
  "uint32" '(x y) '(uint32 uint32) "{return (uint32_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "u64max"
  "uint64" '(x y) '(uint64 uint64) "{return (uint64_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "i8max"
  "int8" '(x y) '(int8 int8) "{return (int8_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "i16max"
  "int16" '(x y) '(int16 int16) "{return (int16_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "i32max"
  "int32" '(x y) '(int32 int32) "{return (int32_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "i64max"
  "int64" '(x y) '(int64 int64) "{return (int64_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "u8min"
  "uint8" '(x y) '(uint8 uint8) "{return (uint8_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "u16min"
  "uint16" '(x y) '(uint16 uint16) "{return (uint16_t)(x< y ? y : x);}")

(def-c-attach-primitive "integertypes" "u32min"
  "uint32" '(x y) '(uint32 uint32) "{return (uint32_t)(x< y ? x : y);}")

(def-c-attach-primitive "integertypes" "u64min"
  "uint64" '(x y) '(uint64 uint64) "{return (uint64_t)(x< y ? x : y);}")

(def-c-attach-primitive "integertypes" "i8min"
  "int8" '(x y) '(int8 int8) "{return (int8_t)(x< y ? x : y);}")

(def-c-attach-primitive "integertypes" "i16min"
  "int16" '(x y) '(int16 int16) "{return (int16_t)(x< y ? x : y);}")

(def-c-attach-primitive "integertypes" "i32min"
  "int32" '(x y) '(int32 int32) "{return (int32_t)(x< y ? x : y);}")

(def-c-attach-primitive "integertypes" "i64min"
  "int64" '(x y) '(int64 int64) "{return (int64_t)(x< y ? x : y);}")

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

(def-c-attach-primitive "integertypes" "u8ceiling"
  "uint8" '(x) '(mpq) "{return (uint8_t)pvsceiling_q_u64(x);}")

(def-c-attach-primitive "integertypes" "u16ceiling"
  "uint16" '(x) '(mpq) "{return (uint16_t)pvsceiling_q_u64(x);}")

(def-c-attach-primitive "integertypes" "u32ceiling"
  "uint32" '(x) '(mpq) "{return (uint32_t)pvsceiling_q_u64(x);}")

(def-c-attach-primitive "integertypes" "u64ceiling"
  "uint64" '(x) '(mpq) "{return (uint64_t)pvsceiling_q_u64(x);}")

(def-c-attach-primitive "integertypes" "i8ceiling"
  "int8" '(x) '(mpq) "{return (int8_t)pvsceiling_q_i64(x);}")

(def-c-attach-primitive "integertypes" "i16ceiling"
  "int16" '(x) '(mpq) "{return (int16_t)pvsceiling_q_i64(x);}")

(def-c-attach-primitive "integertypes" "i32ceiling"
  "int32" '(x) '(mpq) "{return (int32_t)pvsceiling_q_i64(x);}")

(def-c-attach-primitive "integertypes" "i64ceiling"
  "int64" '(x) '(mpq) "{return (int64_t)pvsceiling_q_i64(x);}")

(def-c-attach-primitive "integertypes" "u8floor"
  "uint8" '(x) '(mpq) "{return (uint8_t)pvsfloor_q_u64(x);}")

(def-c-attach-primitive "integertypes" "u16floor"
  "uint16" '(x) '(mpq) "{return (uint16_t)pvsfloor_q_u64(x);}")

(def-c-attach-primitive "integertypes" "u32floor"
  "uint32" '(x) '(mpq) "{return (uint32_t)pvsfloor_q_u64(x);}")

(def-c-attach-primitive "integertypes" "u64floor"
  "uint64" '(x) '(mpq) "{return (uint64_t)pvsfloor_q_u64(x);}")

(def-c-attach-primitive "integertypes" "i8floor"
  "int8" '(x) '(mpq) "{return (int8_t)pvsfloor_q_i64(x);}")

(def-c-attach-primitive "integertypes" "i16floor"
  "int16" '(x) '(mpq) "{return (int16_t)pvsfloor_q_i64(x);}")

(def-c-attach-primitive "integertypes" "i32floor"
  "int32" '(x) '(mpq) "{return (int32_t)pvsfloor_q_i64(x);}")

(def-c-attach-primitive "integertypes" "i64floor"
  "int64" '(x) '(mpq) "{return (int64_t)pvsfloor_q_i64(x);}")

(def-c-attach-primitive "integer_bv_ops" "u8xor" "uint8"
  '(x8 y8) '(uint8 uint8) "{return x8^y8;}")

(def-c-attach-primitive "integer_bv_ops" "u16xor" "uint16"
  '(x16 y16) '(uint16 uint16) "{return x16^y16;}")

(def-c-attach-primitive "integer_bv_ops" "u32xor" "uint32"
  '(x32 y32) '(uint32 uint32) "{return x32^y32;}")

(def-c-attach-primitive "integer_bv_ops" "u64xor" "uint64"
  '(x64 y64) '(uint64 uint64) "{return x64 & y64;}")

(def-c-attach-primitive "integer_bv_ops" "u8and" "uint8"
  '(x8 y8) '(uint8 uint8) "{return x8 & y8;}")

(def-c-attach-primitive "integer_bv_ops" "u16and" "uint16"
  '(x16 y16) '(uint16 uint16) "{return x16 & y16;}")

(def-c-attach-primitive "integer_bv_ops" "u32and" "uint32"
  '(x32 y32) '(uint32 uint32) "{return x32 & y32;}")

(def-c-attach-primitive "integer_bv_ops" "u64and" "uint64"
  '(x64 y64) '(uint64 uint64) "{return x64 & y64;}")

(def-c-attach-primitive "integer_bv_ops" "u8or" "uint8"
  '(x8 y8) '(uint8 uint8) "{return x8 | y8;}")

(def-c-attach-primitive "integer_bv_ops" "u16or" "uint16"
  '(x16 y16) '(uint16 uint16) "{return x16 | y16;}")

(def-c-attach-primitive "integer_bv_ops" "u32or" "uint32"
  '(x32 y32) '(uint32 uint32) "{return x32 | y32;}")

(def-c-attach-primitive "integer_bv_ops" "u64or" "uint64"
  '(x64 y64) '(uint64 uint64) "{return x64 | y64;}")

(def-c-attach-primitive "integer_bv_ops" "u8not" "uint8"
  '(x8) '(uint8 uint8) "{return ~x8;}")

(def-c-attach-primitive "integer_bv_ops" "u16not" "uint16"
  '(x16) '(uint16 uint16) "{return ~x16;}")

(def-c-attach-primitive "integer_bv_ops" "u32not" "uint32"
  '(x32) '(uint32 uint32) "{return ~x32;}")

(def-c-attach-primitive "integer_bv_ops" "u64not" "uint64"
  '(x64) '(uint64 uint64) "{return ~x64;}")

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
;; (def-c-attach-primitive-type "bytestrings" "byte" "uint8")

;; (def-c-attach-primitive-type "bytestrings" "Bytestring" "bytestring_t")

;; (def-c-attach-primitive "bytestrings" "bytestring_bound" "uint64_t" '() '() (expt 2 32))

;; (def-c-attach-primitive "bytestrings" "mk_bytestring" "string" '(s) '(bytestring_t)
;;  (format nil "{~%~8Tuint16_t length = (uint16_t)mpz_get_ui(s->length);~
;; ~%~8T bytestrings_bytestring_t result = (bytestrings_bytestring_t)safe_malloc(sizeof(string_s) + sizeof(length) + 1);~
;; ~%~8T result->count = 1; result->size = length + 1;~
;; ~%~8T uint32_t i;~
;; ~%~8T for (i = 0; i < length; i++){result->strval[i] = s->seq[i];};~
;; ~%~8T result->strval[i] = '\\0'; return result;}"))

;; (def-c-attach-primitive "bytestring" "empty" "bytestring_t" '() '()
;;   (format nil "{~%~8Tbytestring_t result = (bytestring_t)safe_malloc(sizeof(string_s));~
;;  ~%~8T result->count = 1; result->length = 0;~
;;   ~%~8T return result;}"))

;; (def-c-attach-primitive "bytestring" "unit" "bytestring_t" '(c) '(uint8_t)
;;   (format nil "{~%~8Tbytestring_t result = (bytestring_t)safe_malloc(sizeof(string_s) + sizeof(char));~
;;  ~%~8T result->count = 1; result->length = 1; result->strval[0] = c;~
;;   ~%~8T return result;}"))

;; (def-c-attach-primitive "bytestrings" "byte" "bytestrings_byte_t" '(s i) '(bytestring_byte_t)
;;   (format nil "{bytestring_byte_t entry =  s->strval[i];~
;; ~%~8T if (s->count > 1){s->count--;} else {safe_free(s);}~
;; ~%~8T return entry;}"))

;; (def-c-attach-primitive "bytestring" "length" "uint32_t" '(s) '(bytestrings_bytestring)
;;   (format nil "{uint32_t length = (uint32_t)s->length;~
;; ~%~8T if (s->count > 1){s->count--;} else {safe_free(s);}~
;; ~%~8T return length;}"))

;; (def-c-attach-primitive "bytestrings" "+" "bytestrings_bytestring" '(s1 s2) '(bytestrings_bytestring bytestrings_bytestring)
;;   (format nil "{~%~8T uint16_t l1 = s1->length);~
;; ~%~8T uint16_t l2 = s2->length;~
;; ~%~8T if (l1  == 0){return s2;} else {if (l2 == 0) return s2;}
;; ~%~8T uint32_t length = l1 + l2;~
;; ~%~8T result = (bytestrings_bytestring_t)safe_malloc(sizeof(string_s) + length);~
;; ~%~8T result->count = 1;~
;; ~%~8T result->length = length;~
;; ~%~8T memcpy(result->strval, s1->strval, l1);~
;; ~%~8T memcpy(result->strval + l1, s2->strval, l2);~
;; ~%~8Tif (s1->count > 1){s1->count--;} else {safe_free(s1)};~
;; ~%~8Tif (s2->count > 1){s2->count--;} else {safe_free(s2)};~
;; ~%~8Treturn result;~%}"))

;; ;; (def-c-attach-primitive "cstring" "charcmp" "bool" '(c1 c2) '(cstring_cchar cstring_cchar)
;; ;;   "{return c1 < c2;}")

;; (def-c-attach-primitive "bytestrings" "strdiff" "uint32" '(s1 s2) '(bytestrings_bytestring bytestrings_bytestring)
;;   (format nil "{uint32_t i = 0;~%~8Twhile ((i < s1->length) && (i < s2->length) && (s1->strval[i] == s2->strval[i])){i++;};~
;; ~%~8Tif (s1->count > 1){s1->count--;} else {safe_free(s1);}~
;; ~%~8Tif (s2->count > 1){s1->count--;} else {safe_free(s2);}~
;; ~%~8T return i;}"))
  
;; (def-c-attach-primitive "bytestrings" "strcmp" "int8" '(s1 s2) '(bytestrings_bytestring bytestrings_bytestring)
;;   (format nil "{~%~8T int8 cmp = strcmp(s1->strval, s2->strval);~
;; ~%~8Tif (s1->count > 1){s1->count--;} else {safe_free(s1);}~
;; ~%~8Tif (s2->count > 1){s2->count--;} else {safe_free(s2);}~
;; ~%~8Treturn (cmp < 0)? -1 : (cmp > 0) ?  1 : 0;~%}"))

;; (def-c-attach-primitive "bytestrings" "substr" "bytestrings_bytestring" '(s i j) '(bytestrings_bytestring uint32 uint32)
;;   (format nil "{uint32_t length = (i > j) ? i - j : j - i;~
;; ~%~8T uint32_t start = (i > j) ?  j :  i;~
;; ~%~8T bytestrings_bytestring_t result = (bytestrings_bytestring_t)safe_malloc(sizeof(string_s) + length);~
;; ~%~8T result->count = 1; result->size = length;~
;; ~%~8T memcpy(result->strval, (char *)(s + start), length);
;; ~%~8Tif (s->count > 1){s->count--;} else {safe_free(s);}~
;; ~%~8T return result; }"))

;; (def-c-attach-primitive "bytestrings" "prefix" "bytestrings_bytestring" '(s i) '(bytestrings_bytestring uint32)
;;   (format nil "{return bytestrings_substr(s, 0, i);}"))

;; (def-c-attach-primitive "bytestrings" "suffix" "bytestrings_bytestring" '(s i) '(bytestrings_bytestring uint32)
;;   (format nil "{return bytestrings_substr(s, i, s->length);}"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C attachments for pvsio_prelude theories stdstr and stdio

(def-c-attach-primitive "stdstr" "charcode" "string" '(x) '(uint32)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;attachments for file operations
;;(def-c-attach-primitive-type "file" "file_descriptor" "uint64_t")
(def-c-attach-primitive-type "file" "void_action" "uint8_t")
(def-c-attach-primitive-type "file" "file" "file_t")
(def-c-attach-primitive "file" "null_action" "uint8" '() '() "{return 1;}" nil)

(def-c-attach-primitive "file" "file_size" "uint32" '(f) '(file__file)
  nil
  "{uint32_t result = f->size;
    release_file__file(f);
    return result;}")

(def-c-attach-primitive "file" "open?" "bool"
  '(f)
  '(file__file)
  nil
  "{struct stat s;
    return (fstat(f->fd, &s) != -1);
   }")

(def-c-attach-primitive "file" "name" "bytestrings__bytestring"
  '(f)
  '(file__file)
  nil
  "{
   char * name = f->name;
   uint32_t size = strlen(name);
    bytestrings_array_0_t newarray = new_bytestrings_array_0(size);
    memcpy(newarray, (char *) name, size);
    bytestrings__bytestring_t newstring = new_bytestrings__bytestring();
    newstring->length = size;
    newstring->seq = newarray;
    return newstring;
   }")


(def-c-attach-primitive "file" "open" "file__lifted_file_adt"
  '(name)
  '(bytestrings__bytestring)
  nil
  "{
    char * filenamestring = byte2cstring(name->length, name->seq->elems);
    uint64_t fd = open(filenamestring, O_RDWR, S_IRUSR | S_IWUSR);
    release_bytestrings__bytestring(name); 
    safe_free(filenamestring);
    struct stat s;
    if (fstat(fd, &s) == -1){
       return file__fail(); //pvs2cerror(\"File size extraction failed.\n\")
       }
    uint32_t size = s.st_size;
    uint32_t capacity = 4096 * (size/4096 + 1);
    char * contents = (char *) mmap(NULL, capacity, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    file_t ff = (file_t) safe_malloc(sizeof(file_s));
    ff->count = 1;
    ff->fd = fd;
    ff->size = size;
    ff->capacity = capacity;
    ff->contents = contents;
    ff->name = filenamestring;
    return file__pass(ff);
   }")

(def-c-attach-primitive "file" "append" "file__lifted_file_adt" '(f b) '(file__file bytestrings__bytestring)
  nil
  "{uint64_t fd = f->fd;
    uint32_t size = f->size;
    uint32_t capacity = f->capacity;
    char * contents = f->contents;
    uint32_t len = b->length;
    char * data = (char *)b->seq->elems;
    ftruncate(fd, size + len);
    if (size + len < capacity){
    for (size_t i = 0; i < len; i++)
      contents[i + size] = data[i];
  } else {
    munmap(contents, capacity);
    uint32_t new_capacity = capacity + (10 * 4096);
    char * new_contents = mmap(NULL, new_capacity, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0); 
    for (size_t i = 0; i < len; i++)
      new_contents[i + size] = data[i];
    f->contents = new_contents;
 };
   f->size = size + len;
   return file__pass(f);
}")

(def-c-attach-primitive "file" "getbyte" "uint8" '(f i) '(file__file uint32)
  nil
  "{uint8_t result = f->contents[i];
    release_file__file(f);
    return result;
}")

(def-c-attach-primitive "file" "getbytestring" "bytestrings__bytestring" '(f i size) '(file__file uint32 uint32)
  nil
  "{bytestrings_array_0_t newarray = new_bytestrings_array_0(size);
    memcpy(newarray, (char *) f->contents + i, size);
    bytestrings__bytestring_t newstring = new_bytestrings__bytestring();
    newstring->length = size;
    newstring->seq = newarray;
    release_file__file(f);
    return newstring;
    }")


;;(6-3-22): Need to add reference count to file type and treat it as a reference type in the IR
(def-c-attach-primitive "file" "setbyte" "file__file" '(f i b) '(file__file uint32 uint8)
  nil
  "{if (f->count == 1){
     f->contents[i] = b;
     };
    return f;
}")

(def-c-attach-primitive "file" "printc" "bytestrings__bytestring" '(b) '(bytestrings__bytestring) nil
  "{printf(\"\\n\");
    for (uint32_t i = 0; i < b->length; i++) printf(\"%c\", b->seq->elems[i]);
    return b;
   }
")


(def-c-attach-primitive "file" "printh" "bytestrings__bytestring" '(b) '(bytestrings__bytestring) nil
  "{printf(\"\\n\");
    for (uint32_t i = 0; i < b->length; i++) printf(\"%02X\", b->seq->elems[i]);
    return b;
   }
")


















