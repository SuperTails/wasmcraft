# PushFrame(9)
# Push frame with 9 locals
execute at @e[tag=frameptr] run fill ~ ~ ~ ~8 ~ ~1 minecraft:jukebox{RecordItem:{id:"minecraft:stone",Count:1b,tag:{Memory:0}}}
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~9 ~ ~
# Comment("#   Parameter 0")
# #   Parameter 0
# StoreLocalI64(Param(0), 0)
execute at @e[tag=frameptr] store result block ~-1 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %param%0%lo reg
execute at @e[tag=frameptr] store result block ~-1 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %param%0%hi reg
# Comment("#   Parameter 1")
# #   Parameter 1
# StoreLocalI64(Param(1), 1)
execute at @e[tag=frameptr] store result block ~-2 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %param%1%lo reg
execute at @e[tag=frameptr] store result block ~-2 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %param%1%hi reg
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# Comment("LocalSet { local_index: 2 }")
# LocalSet { local_index: 2 }
# SetConst(HalfRegister(Work(0, 2), Lo), 63)
scoreboard players set %work%0%lo%2%temp reg 63
# SetConst(HalfRegister(Work(0, 2), Hi), 0)
scoreboard players set %work%0%hi%2%temp reg 0
# StoreLocalI64(Work(0, 2), 2)
execute at @e[tag=frameptr] store result block ~-3 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%2%temp reg
execute at @e[tag=frameptr] store result block ~-3 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%2%temp reg
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# LoadLocalI64(Work(0, 2), 1)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-2 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-2 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# LoadLocalI64(Work(0, 2), 1)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-2 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-2 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# Comment("I64ShrS")
# I64ShrS
# SetConst(HalfRegister(Work(1, 2), Lo), 63)
scoreboard players set %work%1%lo%2%temp reg 63
# SetConst(HalfRegister(Work(1, 2), Hi), 0)
scoreboard players set %work%1%hi%2%temp reg 0
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%1%hi%2%temp reg
# I64ShrS { dst: Work(2, 2), lhs: Work(0, 2), rhs: Work(1, 2) }
scoreboard players operation %param0%0 reg = %work%0%lo%2%temp reg
scoreboard players operation %param0%1 reg = %work%0%hi%2%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%2%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:ashr_i64
scoreboard players operation %work%2%lo%2%temp reg = %param0%0 reg
scoreboard players operation %work%2%hi%2%temp reg = %param0%1 reg
# PushI64From(Work(2, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%2%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%2%hi%2%temp reg
# Comment("LocalTee { local_index: 3 }")
# LocalTee { local_index: 3 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%1%hi%2%temp reg
# PushI64From(Work(0, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%0%hi%2%temp reg
# PushI64From(Work(0, 2))
scoreboard players operation %stack%2%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%2%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%2%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%2%hi%2%temp reg
# StoreLocalI64(Work(0, 2), 3)
execute at @e[tag=frameptr] store result block ~-4 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%2%temp reg
execute at @e[tag=frameptr] store result block ~-4 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%2%temp reg
# Comment("I64Add")
# I64Add
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%1%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# I64Add { dst: Work(2, 2), lhs: Work(0, 2), rhs: Work(1, 2) }
scoreboard players operation %work%2%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %work%2%hi%2%temp reg = %work%0%hi%2%temp reg
scoreboard players operation %work%2%lo%2%temp reg += %work%1%lo%2%temp reg
scoreboard players operation %work%2%hi%2%temp reg += %work%1%hi%2%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%0%lo%2%temp reg matches ..-1 if score %work%1%lo%2%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%2%temp reg matches ..-1 if score %work%1%lo%2%temp reg matches 0.. if score %work%2%lo%2%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%2%temp reg matches 0.. if score %work%1%lo%2%temp reg matches ..-1 if score %work%2%lo%2%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%hi%2%temp reg += %temp%10%lo reg
# PushI64From(Work(2, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%2%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%2%hi%2%temp reg
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# LoadLocalI64(Work(0, 2), 3)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-4 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-4 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Xor")
# I64Xor
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%1%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# I32Op { dst: HalfRegister(Work(2, 2), Lo), lhs: HalfRegister(Work(0, 2), Lo), op: "^=", rhs: Reg(HalfRegister(Work(1, 2), Lo)) }
scoreboard players operation %param0%0 reg = %work%0%lo%2%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%2%temp reg
function intrinsic:xor
scoreboard players operation %work%2%lo%2%temp reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2, 2), Hi), lhs: HalfRegister(Work(0, 2), Hi), op: "^=", rhs: Reg(HalfRegister(Work(1, 2), Hi)) }
scoreboard players operation %param0%0 reg = %work%0%hi%2%temp reg
scoreboard players operation %param1%0 reg = %work%1%hi%2%temp reg
function intrinsic:xor
scoreboard players operation %work%2%hi%2%temp reg = %return%0 reg
# PushI64From(Work(2, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%2%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%2%hi%2%temp reg
# Comment("LocalSet { local_index: 4 }")
# LocalSet { local_index: 4 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# StoreLocalI64(Work(0, 2), 4)
execute at @e[tag=frameptr] store result block ~-5 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%2%temp reg
execute at @e[tag=frameptr] store result block ~-5 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%2%temp reg
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# LoadLocalI64(Work(0, 2), 0)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-1 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-1 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# LoadLocalI64(Work(0, 2), 0)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-1 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-1 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# PushI64Const(63)
scoreboard players set %stack%2%lo%2%temp reg 63
scoreboard players set %stack%2%hi%2%temp reg 0
# Comment("I64ShrS")
# I64ShrS
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%2%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%2%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%1%hi%2%temp reg
# I64ShrS { dst: Work(2, 2), lhs: Work(0, 2), rhs: Work(1, 2) }
scoreboard players operation %param0%0 reg = %work%0%lo%2%temp reg
scoreboard players operation %param0%1 reg = %work%0%hi%2%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%2%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:ashr_i64
scoreboard players operation %work%2%lo%2%temp reg = %param0%0 reg
scoreboard players operation %work%2%hi%2%temp reg = %param0%1 reg
# PushI64From(Work(2, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%2%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%2%hi%2%temp reg
# Comment("LocalTee { local_index: 3 }")
# LocalTee { local_index: 3 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%1%hi%2%temp reg
# PushI64From(Work(0, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%0%hi%2%temp reg
# PushI64From(Work(0, 2))
scoreboard players operation %stack%2%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%2%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%2%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%2%hi%2%temp reg
# StoreLocalI64(Work(0, 2), 3)
execute at @e[tag=frameptr] store result block ~-4 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%2%temp reg
execute at @e[tag=frameptr] store result block ~-4 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%2%temp reg
# Comment("I64Add")
# I64Add
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%1%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# I64Add { dst: Work(2, 2), lhs: Work(0, 2), rhs: Work(1, 2) }
scoreboard players operation %work%2%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %work%2%hi%2%temp reg = %work%0%hi%2%temp reg
scoreboard players operation %work%2%lo%2%temp reg += %work%1%lo%2%temp reg
scoreboard players operation %work%2%hi%2%temp reg += %work%1%hi%2%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%0%lo%2%temp reg matches ..-1 if score %work%1%lo%2%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%2%temp reg matches ..-1 if score %work%1%lo%2%temp reg matches 0.. if score %work%2%lo%2%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%2%temp reg matches 0.. if score %work%1%lo%2%temp reg matches ..-1 if score %work%2%lo%2%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%hi%2%temp reg += %temp%10%lo reg
# PushI64From(Work(2, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%2%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%2%hi%2%temp reg
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# LoadLocalI64(Work(0, 2), 3)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-4 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-4 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%1%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%1%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Xor")
# I64Xor
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%1%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# I32Op { dst: HalfRegister(Work(2, 2), Lo), lhs: HalfRegister(Work(0, 2), Lo), op: "^=", rhs: Reg(HalfRegister(Work(1, 2), Lo)) }
scoreboard players operation %param0%0 reg = %work%0%lo%2%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%2%temp reg
function intrinsic:xor
scoreboard players operation %work%2%lo%2%temp reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2, 2), Hi), lhs: HalfRegister(Work(0, 2), Hi), op: "^=", rhs: Reg(HalfRegister(Work(1, 2), Hi)) }
scoreboard players operation %param0%0 reg = %work%0%hi%2%temp reg
scoreboard players operation %param1%0 reg = %work%1%hi%2%temp reg
function intrinsic:xor
scoreboard players operation %work%2%hi%2%temp reg = %return%0 reg
# PushI64From(Work(2, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%2%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%2%hi%2%temp reg
# Comment("LocalSet { local_index: 5 }")
# LocalSet { local_index: 5 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# StoreLocalI64(Work(0, 2), 5)
execute at @e[tag=frameptr] store result block ~-6 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%2%temp reg
execute at @e[tag=frameptr] store result block ~-6 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%2%temp reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%0%lo%2%temp reg 0
scoreboard players set %stack%0%hi%2%temp reg 0
# Comment("LocalSet { local_index: 6 }")
# LocalSet { local_index: 6 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# StoreLocalI64(Work(0, 2), 6)
execute at @e[tag=frameptr] store result block ~-7 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%2%temp reg
execute at @e[tag=frameptr] store result block ~-7 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%2%temp reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%0%lo%2%temp reg 0
scoreboard players set %stack%0%hi%2%temp reg 0
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# StoreLocalI64(Work(0, 2), 3)
execute at @e[tag=frameptr] store result block ~-4 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%2%temp reg
execute at @e[tag=frameptr] store result block ~-4 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%2%temp reg
# Comment("Loop { ty: Type(EmptyBlockType) }")
# Loop { ty: Type(EmptyBlockType) }
#   Branch to __wasm2_3
#   Jump to __wasm2_3
function intrinsic:i64divrem/__wasm2_3
scoreboard players set %%taken wasm 1