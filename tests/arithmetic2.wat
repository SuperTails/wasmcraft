(module
  (type (;0;) (func (result i32)))
  (func $_start (type 0) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    (; l[0] = g[0] ;)
    global.get 0
    local.set 0

    (; l[1] = 16 ;)
    i32.const 16
    local.set 1

    (; l[2] = l[0] - l[1] ;)
    local.get 0
    local.get 1
    i32.sub
    local.set 2

    (; l[3] = 7 ;)
    i32.const 7
    local.set 3

    (; l[4] = 5 ;)
    i32.const 5
    local.set 4

    (; m[l[2] + 12] = l[4] ;)
    local.get 2
    local.get 4
    i32.store offset=12

    (; m[l[2] + 8] = l[3] ;)
    local.get 2
    local.get 3
    i32.store offset=8

    (; l[5] = m[l[2] + 12] ;)
    local.get 2
    i32.load offset=12
    local.set 5

    (; l[6] = m[l[2] + 8] ;)
    local.get 2
    i32.load offset=8
    local.set 6

    (; l[7] = 2 ;)
    i32.const 2
    local.set 7

    (; l[8] = l[6] << l[7] ;)
    local.get 6
    local.get 7
    i32.shl
    local.set 8

    (; l[9] = l[5] - l[8] ;)
    local.get 5
    local.get 8
    i32.sub
    local.set 9

    (; l[10] = 2 ;)
    i32.const 2
    local.set 10

    (; l[11] = l[9] / l[10] ;)
    local.get 9
    local.get 10
    i32.div_s
    local.set 11

    (; return l[11] ;)
    local.get 11
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
