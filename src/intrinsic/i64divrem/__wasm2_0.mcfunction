# PushFrame(9)
# Push frame with 9 locals
execute at @e[tag=frameptr] run fill ~ ~ ~ ~8 ~ ~1 minecraft:jukebox{RecordItem:{id:"minecraft:stone",Count:1b,tag:{Memory:0}}}
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~9 ~ ~
# Comment("#   Parameter 0")
# #   Parameter 0
# SetLocalPtr(0)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-1 0 1
# StoreLocalI64(Param(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %param%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %param%0%hi reg
# Comment("#   Parameter 1")
# #   Parameter 1
# SetLocalPtr(1)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-2 0 1
# StoreLocalI64(Param(1))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %param%1%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %param%1%hi reg
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# PushI64Const(63)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 63
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalSet { local_index: 2 }")
# LocalSet { local_index: 2 }
# SetLocalPtr(2)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-3 0 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# SetLocalPtr(1)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-2 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# SetLocalPtr(1)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-2 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# PushI64Const(63)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 63
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64ShrS")
# I64ShrS
# PopI64Into(Work(1))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# I64ShrS { dst: Work(2), lhs: Work(0), rhs: Work(1) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param0%1 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
function intrinsic:ashr_i64
scoreboard players operation %work%2%lo reg = %param0%0 reg
scoreboard players operation %work%2%hi reg = %param0%1 reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalTee { local_index: 3 }")
# LocalTee { local_index: 3 }
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("I64Add")
# I64Add
# PopI64Into(Work(1))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# I64Add { dst: Work(2), lhs: Work(0), rhs: Work(1) }
scoreboard players operation %work%2%lo reg = %work%0%lo reg
scoreboard players operation %work%2%hi reg = %work%0%hi reg
scoreboard players operation %work%2%lo reg += %work%1%lo reg
scoreboard players operation %work%2%hi reg += %work%1%hi reg
scoreboard players set %work%10%lo reg 0
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches ..-1 run scoreboard players set %work%10%lo reg 1
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches 0.. if score %work%2%lo reg matches 0.. run scoreboard players set %work%10%lo reg 1
execute if score %work%0%lo reg matches 0.. if score %work%1%lo reg matches ..-1 if score %work%2%lo reg matches 0.. run scoreboard players set %work%10%lo reg 1
scoreboard players operation %work%2%hi reg += %work%10%lo reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Xor")
# I64Xor
# PopI64Into(Work(1))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# I32Op { dst: HalfRegister(Work(2), Lo), lhs: HalfRegister(Work(0), Lo), op: "^=", rhs: HalfRegister(Work(1), Lo) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
function intrinsic:xor
scoreboard players operation %work%2%lo reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2), Hi), lhs: HalfRegister(Work(0), Hi), op: "^=", rhs: HalfRegister(Work(1), Hi) }
scoreboard players operation %param0%0 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%hi reg
function intrinsic:xor
scoreboard players operation %work%2%hi reg = %return%0 reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalSet { local_index: 4 }")
# LocalSet { local_index: 4 }
# SetLocalPtr(4)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-5 0 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# SetLocalPtr(0)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-1 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# SetLocalPtr(0)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-1 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# PushI64Const(63)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 63
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64ShrS")
# I64ShrS
# PopI64Into(Work(1))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# I64ShrS { dst: Work(2), lhs: Work(0), rhs: Work(1) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param0%1 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
function intrinsic:ashr_i64
scoreboard players operation %work%2%lo reg = %param0%0 reg
scoreboard players operation %work%2%hi reg = %param0%1 reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalTee { local_index: 3 }")
# LocalTee { local_index: 3 }
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("I64Add")
# I64Add
# PopI64Into(Work(1))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# I64Add { dst: Work(2), lhs: Work(0), rhs: Work(1) }
scoreboard players operation %work%2%lo reg = %work%0%lo reg
scoreboard players operation %work%2%hi reg = %work%0%hi reg
scoreboard players operation %work%2%lo reg += %work%1%lo reg
scoreboard players operation %work%2%hi reg += %work%1%hi reg
scoreboard players set %work%10%lo reg 0
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches ..-1 run scoreboard players set %work%10%lo reg 1
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches 0.. if score %work%2%lo reg matches 0.. run scoreboard players set %work%10%lo reg 1
execute if score %work%0%lo reg matches 0.. if score %work%1%lo reg matches ..-1 if score %work%2%lo reg matches 0.. run scoreboard players set %work%10%lo reg 1
scoreboard players operation %work%2%hi reg += %work%10%lo reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Xor")
# I64Xor
# PopI64Into(Work(1))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%1%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# I32Op { dst: HalfRegister(Work(2), Lo), lhs: HalfRegister(Work(0), Lo), op: "^=", rhs: HalfRegister(Work(1), Lo) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
function intrinsic:xor
scoreboard players operation %work%2%lo reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2), Hi), lhs: HalfRegister(Work(0), Hi), op: "^=", rhs: HalfRegister(Work(1), Hi) }
scoreboard players operation %param0%0 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%hi reg
function intrinsic:xor
scoreboard players operation %work%2%hi reg = %return%0 reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalSet { local_index: 5 }")
# LocalSet { local_index: 5 }
# SetLocalPtr(5)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-6 0 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalSet { local_index: 6 }")
# LocalSet { local_index: 6 }
# SetLocalPtr(6)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-7 0 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# PopI64Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("Loop { ty: Type(EmptyBlockType) }")
# Loop { ty: Type(EmptyBlockType) }
# RawBranch(Label { func_idx: CodeFuncIdx(2), idx: 3 })
#   Jump to __wasm2_3
function intrinsic:i64divrem/__wasm2_3