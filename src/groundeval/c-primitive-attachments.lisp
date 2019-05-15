(clrhash *c-primitive-attachments-hash*)
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




