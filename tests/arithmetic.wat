(module
  (type (;0;) (func (result i32)))
  (func $_start (type 0) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    local.set 0
    i32.const 16
    local.set 1
    local.get 0
    local.get 1
    i32.sub
    local.set 2
    i32.const 0
    local.set 3
    i32.const 7
    local.set 4
    i32.const 5
    local.set 5
    i32.const 3
    local.set 6
    local.get 2
    local.get 6
    i32.store offset=12
    local.get 2
    local.get 5
    i32.store offset=8
    local.get 2
    local.get 4
    i32.store offset=4
    local.get 2
    i32.load offset=8
    local.set 7
    local.get 3
    local.get 7
    i32.sub
    local.set 8
    local.get 2
    i32.load offset=8
    local.set 9
    local.get 2
    i32.load offset=8
    local.set 10
    local.get 9
    local.get 10
    i32.mul
    local.set 11
    local.get 2
    i32.load offset=12
    local.set 12
    i32.const 2
    local.set 13
    local.get 12
    local.get 13
    i32.shl
    local.set 14
    local.get 2
    i32.load offset=4
    local.set 15
    local.get 14
    local.get 15
    i32.mul
    local.set 16
    local.get 11
    local.get 16
    i32.sub
    local.set 17
    i32.const 2
    local.set 18
    local.get 17
    local.get 18
    i32.div_s
    local.set 19
    local.get 2
    i32.load offset=12
    local.set 20
    local.get 19
    local.get 20
    i32.mul
    local.set 21
    local.get 8
    local.get 21
    i32.add
    local.set 22
    local.get 22
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
