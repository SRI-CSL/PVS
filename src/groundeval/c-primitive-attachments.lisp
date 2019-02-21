
(def-c-attach-primitive "integertypes" "u8plus" 
  "uint8" "(uint8_t x, uint8_t y)" "{return (uint8_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u16plus" 
  "uint16" "(uint16_t x, uint16_t y)" "{return (uint16_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u32plus" 
  "uint32"  "(uint32_t x, uint32_t y)" "{return (uint32_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u64plus" 
  "uint64"  "(uint64_t x, uint64_t y)" "{return (uint64_t)(x + y);}")

(def-c-attach-primitive "integertypes" "u8minus"
  "uint8" "(uint8_t x, uint8_t y)" "{return (uint8_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u16minus"
  "uint16" "(uint16_t x, uint16_t y)" "{return (uint16_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u32minus" 
  "uint32" "(uint32_t x, uint32_t y)" "{return (uint32_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u64minus"
  "uint64" "(uint64_t x, uint64_t y)" "{return (uint64_t)(x - y);}")

(def-c-attach-primitive "integertypes" "u8times" 
  "uint8" "(uint8_t x, uint8_t y)" "{return (uint8_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u16times"
  "uint16" "(uint16_t x, uint16_t y)" "{return (uint16_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u32times" 
  "uint32" "(uint32_t x, uint32_t y)" "{return (uint32_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u64times" 
  "uint64" "(uint64_t x, uint64_t y)" "{return (uint64_t)(x * y);}")

(def-c-attach-primitive "integertypes" "u8div" 
  "uint8" "(uint8_t x, uint8_t y)" "{return (uint8_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u16div" 
  "uint16" "(uint16_t x, uint16_t y)" "{return (uint16_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u32div" 
  "uint32"  "(uint32_t x, uint32_t y)" "{return (uint32_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u64div" 
  "uint64" "(uint64_t x, uint64_t y)" "{return (uint64_t)(x/y);}")

(def-c-attach-primitive "integertypes" "u8rem" 
  "uint8"  "(uint8_t x, uint8_t y)" "{return (uint8_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u16rem" 
  "uint16" "(uint16_t x, uint16_t y)" "{return (uint16_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u32rem"
  "uint32" "(uint32_t x, uint32_t y)" "{return (uint32_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u64rem"
  "uint64" "(uint64_t x, uint64_t y)" "{return (uint64_t)(x%y);}")

(def-c-attach-primitive "integertypes" "u8pow2"
  "uint8"  "(uint8_t x)" "{return (uint8_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u16pow2" 
  "uint16" "(uint16_t x)" "{return (uint16_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u32pow2"
  "uint32" "(uint32_t x)" "{return (uint32_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u64pow2"
  "uint64" "(uint64_t x)" "{return (uint64_t)1<<x;}")

(def-c-attach-primitive "integertypes" "u8lshift"
  "uint8" "(uint8_t x, uint8_t n)" "{return (uint8_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u16lshift"
  "uint16" "(uint16_t x, uint16_t n)" "{return (uint16_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u32lshift"
  "uint32" "(uint32_t x, uint32_t n)" "{return (uint32_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u64lshift"
  "uint64" "(uint64_t x, uint64_t n)" "{return (uint64_t)x<<n;}")

(def-c-attach-primitive "integertypes" "u8rshift"
  "uint8"  "(uint8_t x, uint8_t n)" "{return (uint8_t)x>>n;}")

(def-c-attach-primitive "integertypes" "u16rshift" 
  "uint16" "(uint16_t x, uint16_t n)" "{return (uint16_t)x>>n;}")

(def-c-attach-primitive "integertypes" "u32rshift"
  "uint32" "(uint32_t x, uint32_t n)" "{return (uint32_t)x>>n;}")

(def-c-attach-primitive "integertypes" "u64rshift"
  "uint64" "(uint64_t x, uint64_t n)" "{return (uint64_t)x>>n;}")




