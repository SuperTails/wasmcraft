(module
  (type (;0;) (func (param i32 i32 i32) (result i32)))
  (type (;1;) (func (result i32)))
  (func $do_stuff (type 0) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)

    (; memory[0] = 42 ;)
    i32.const 0
    i32.const 42
    i32.store offset=0

    (; return memory[0] ;)
    i32.const 0
    i32.load offset=0
    return)
  (func $_start (type 1) (result i32)
    (local i32 i32 i32 i32)

    (; local[0] = 3 ;)
    i32.const 3
    local.set 0

    (; local[1] = 5 ;)
    i32.const 5
    local.set 1

    (; local[2] = 7 ;)
    i32.const 7
    local.set 2

    (; local[3] = do_stuff(local[0], local[1], local[2]) ;)
    local.get 0
    local.get 1
    local.get 2
    call $do_stuff
    local.set 3

    (; return local[3] ;)
    local.get 3
    return)
  (table (;0;) 1 1 funcref)
  (memory (;0;) 2)
  (global (;0;) (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "_start" (func $_start)))
