(module
  (type (;0;) (func (result i32)))
  (func $_start (type 0) (result i32)
    i32.const 42)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
