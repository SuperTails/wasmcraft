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
# Comment("I64Const { value: 1 }")
# I64Const { value: 1 }
# PushI64Const(1)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Shl")
# I64Shl
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
# I64Shl { dst: Work(2), lhs: Work(0), rhs: Work(1) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param0%1 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%2%lo reg = %param0%0 reg
scoreboard players operation %work%2%hi reg = %param0%1 reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Const { value: 1 }")
# I64Const { value: 1 }
# PushI64Const(1)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalGet { local_index: 2 }")
# LocalGet { local_index: 2 }
# SetLocalPtr(2)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-3 0 1
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
# Comment("I64Shl")
# I64Shl
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
# I64Shl { dst: Work(2), lhs: Work(0), rhs: Work(1) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param0%1 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%2%lo reg = %param0%0 reg
scoreboard players operation %work%2%hi reg = %param0%1 reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalTee { local_index: 5 }")
# LocalTee { local_index: 5 }
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
# Comment("I64And")
# I64And
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
# I32Op { dst: HalfRegister(Work(2), Lo), lhs: HalfRegister(Work(0), Lo), op: "&=", rhs: HalfRegister(Work(1), Lo) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
function intrinsic:and
scoreboard players operation %work%2%lo reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2), Hi), lhs: HalfRegister(Work(0), Hi), op: "&=", rhs: HalfRegister(Work(1), Hi) }
scoreboard players operation %param0%0 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%hi reg
function intrinsic:and
scoreboard players operation %work%2%hi reg = %return%0 reg
# PushI64From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Ne")
# I64Ne
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
# I64Eq { dst: Work(2), lhs: Work(0), invert: true, rhs: Work(1) }
execute store success score %work%2%lo reg if score %work%0%lo reg = %work%1%lo reg if score %work%0%hi reg = %work%1%hi reg
execute store success score %work%2%lo reg if score %work%2%lo reg matches 0..0
# PushI32From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~
scoreboard players add %stackptr wasm 2
# Comment("I64ExtendI32U")
# I64ExtendI32U
# PopI32Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~
scoreboard players remove %stackptr wasm 2
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# I64ExtendI32U(Work(0))
scoreboard players set %work%0%hi reg 0
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Or")
# I64Or
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
# I32Op { dst: HalfRegister(Work(2), Lo), lhs: HalfRegister(Work(0), Lo), op: "|=", rhs: HalfRegister(Work(1), Lo) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
function intrinsic:or
scoreboard players operation %work%2%lo reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2), Hi), lhs: HalfRegister(Work(0), Hi), op: "|=", rhs: HalfRegister(Work(1), Hi) }
scoreboard players operation %param0%0 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%hi reg
function intrinsic:or
scoreboard players operation %work%2%hi reg = %return%0 reg
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
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
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
# Comment("I64LtU")
# I64LtU
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
# I64UComp { dst: Work(2), lhs: Work(0), op: LessThan, rhs: Work(1) }
# I32Op { dst: HalfRegister(Work(3), Lo), lhs: HalfRegister(Work(0), Hi), op: "ltu", rhs: HalfRegister(Work(1), Hi) }
scoreboard players set %work%3%lo reg 0
execute if score %work%0%hi reg matches ..-1 if score %work%1%hi reg matches 0.. run scoreboard players set %work%3%lo reg 0
execute if score %work%0%hi reg matches 0.. if score %work%1%hi reg matches ..-1 run scoreboard players set %work%3%lo reg 1
execute if score %work%0%hi reg matches ..-1 if score %work%1%hi reg matches ..-1 if score %work%0%hi reg < %work%1%hi reg run scoreboard players set %work%3%lo reg 1
execute if score %work%0%hi reg matches 0.. if score %work%1%hi reg matches 0.. if score %work%0%hi reg < %work%1%hi reg run scoreboard players set %work%3%lo reg 1
# I32Op { dst: HalfRegister(Work(4), Lo), lhs: HalfRegister(Work(0), Hi), op: "gtu", rhs: HalfRegister(Work(1), Hi) }
scoreboard players set %work%4%lo reg 0
execute if score %work%1%hi reg matches ..-1 if score %work%0%hi reg matches 0.. run scoreboard players set %work%4%lo reg 0
execute if score %work%1%hi reg matches 0.. if score %work%0%hi reg matches ..-1 run scoreboard players set %work%4%lo reg 1
execute if score %work%1%hi reg matches ..-1 if score %work%0%hi reg matches ..-1 if score %work%1%hi reg < %work%0%hi reg run scoreboard players set %work%4%lo reg 1
execute if score %work%1%hi reg matches 0.. if score %work%0%hi reg matches 0.. if score %work%1%hi reg < %work%0%hi reg run scoreboard players set %work%4%lo reg 1
# I32Op { dst: HalfRegister(Work(5), Lo), lhs: HalfRegister(Work(0), Hi), op: "==", rhs: HalfRegister(Work(1), Hi) }
execute store success score %work%5%lo reg if score %work%0%hi reg = %work%1%hi reg
# I32Op { dst: HalfRegister(Work(6), Lo), lhs: HalfRegister(Work(0), Lo), op: "ltu", rhs: HalfRegister(Work(1), Lo) }
scoreboard players set %work%6%lo reg 0
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches 0.. run scoreboard players set %work%6%lo reg 0
execute if score %work%0%lo reg matches 0.. if score %work%1%lo reg matches ..-1 run scoreboard players set %work%6%lo reg 1
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches ..-1 if score %work%0%lo reg < %work%1%lo reg run scoreboard players set %work%6%lo reg 1
execute if score %work%0%lo reg matches 0.. if score %work%1%lo reg matches 0.. if score %work%0%lo reg < %work%1%lo reg run scoreboard players set %work%6%lo reg 1
execute if score %work%3%lo reg matches 1.. run scoreboard players set %work%2%lo reg 1
execute if score %work%4%lo reg matches 1.. run scoreboard players set %work%2%lo reg 0
execute if score %work%5%lo reg matches 1.. run scoreboard players operation %work%2%lo reg = %work%6%lo reg
# PushI32From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~
scoreboard players add %stackptr wasm 2
# Comment("LocalTee { local_index: 6 }")
# LocalTee { local_index: 6 }
# PopI32Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~
scoreboard players remove %stackptr wasm 2
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PushI32From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~
scoreboard players add %stackptr wasm 2
# PushI32From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~
scoreboard players add %stackptr wasm 2
# Comment("LocalSet { local_index: 6 }")
# LocalSet { local_index: 6 }
# SetLocalPtr(6)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-7 0 1
# PopI32Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~
scoreboard players remove %stackptr wasm 2
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# StoreLocalI32(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
# Comment("Select")
# Select
# PopI32Into(Work(2))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~
scoreboard players remove %stackptr wasm 2
execute at @e[tag=stackptr] store result score %work%2%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
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
# SelectI64 { dst_reg: Work(0), true_reg: Work(0), false_reg: Work(1), cond_reg: Work(2) }
scoreboard players operation %work%0%lo reg = %work%0%lo reg
scoreboard players operation %work%0%hi reg = %work%0%hi reg
execute if score %work%2%lo reg matches 0..0 run scoreboard players operation %work%0%lo reg = %work%1%lo reg
execute if score %work%2%lo reg matches 0..0 run scoreboard players operation %work%0%hi reg = %work%1%hi reg
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Sub")
# I64Sub
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
# SetConst(HalfRegister(Work(3), Lo), -1)
scoreboard players set %work%3%lo reg -1
# SetConst(HalfRegister(Work(4), Lo), 1)
scoreboard players set %work%4%lo reg 1
# SetConst(HalfRegister(Work(4), Hi), 0)
scoreboard players set %work%4%hi reg 0
# I32Op { dst: HalfRegister(Work(1), Lo), lhs: HalfRegister(Work(1), Lo), op: "*=", rhs: HalfRegister(Work(3), Lo) }
scoreboard players operation %work%1%lo reg *= %work%3%lo reg
# AddI32Const(HalfRegister(Work(1), Lo), -1)
scoreboard players remove %work%1%lo reg 1
# I32Op { dst: HalfRegister(Work(1), Hi), lhs: HalfRegister(Work(1), Hi), op: "*=", rhs: HalfRegister(Work(3), Lo) }
scoreboard players operation %work%1%hi reg *= %work%3%lo reg
# AddI32Const(HalfRegister(Work(1), Hi), -1)
scoreboard players remove %work%1%hi reg 1
# I64Add { dst: Work(5), lhs: Work(1), rhs: Work(4) }
scoreboard players operation %work%5%lo reg = %work%1%lo reg
scoreboard players operation %work%5%hi reg = %work%1%hi reg
scoreboard players operation %work%5%lo reg += %work%4%lo reg
scoreboard players operation %work%5%hi reg += %work%4%hi reg
scoreboard players set %work%10%lo reg 0
execute if score %work%1%lo reg matches ..-1 if score %work%4%lo reg matches ..-1 run scoreboard players set %work%10%lo reg 1
execute if score %work%1%lo reg matches ..-1 if score %work%4%lo reg matches 0.. if score %work%5%lo reg matches 0.. run scoreboard players set %work%10%lo reg 1
execute if score %work%1%lo reg matches 0.. if score %work%4%lo reg matches ..-1 if score %work%5%lo reg matches 0.. run scoreboard players set %work%10%lo reg 1
scoreboard players operation %work%5%hi reg += %work%10%lo reg
# Copy { dst: HalfRegister(Work(1), Lo), src: HalfRegister(Work(5), Lo) }
scoreboard players operation %work%1%lo reg = %work%5%lo reg
# Copy { dst: HalfRegister(Work(1), Hi), src: HalfRegister(Work(5), Hi) }
scoreboard players operation %work%1%hi reg = %work%5%hi reg
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
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value 0
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalGet { local_index: 5 }")
# LocalGet { local_index: 5 }
# SetLocalPtr(5)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-6 0 1
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
# Comment("LocalGet { local_index: 6 }")
# LocalGet { local_index: 6 }
# SetLocalPtr(6)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-7 0 1
# LoadLocalI32(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# PushI32From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~
scoreboard players add %stackptr wasm 2
# Comment("Select")
# Select
# PopI32Into(Work(2))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~
scoreboard players remove %stackptr wasm 2
execute at @e[tag=stackptr] store result score %work%2%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
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
# SelectI64 { dst_reg: Work(0), true_reg: Work(0), false_reg: Work(1), cond_reg: Work(2) }
scoreboard players operation %work%0%lo reg = %work%0%lo reg
scoreboard players operation %work%0%hi reg = %work%0%hi reg
execute if score %work%2%lo reg matches 0..0 run scoreboard players operation %work%0%lo reg = %work%1%lo reg
execute if score %work%2%lo reg matches 0..0 run scoreboard players operation %work%0%hi reg = %work%1%hi reg
# PushI64From(Work(0))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("LocalGet { local_index: 4 }")
# LocalGet { local_index: 4 }
# SetLocalPtr(4)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-5 0 1
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
# Comment("I64Or")
# I64Or
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
# I32Op { dst: HalfRegister(Work(2), Lo), lhs: HalfRegister(Work(0), Lo), op: "|=", rhs: HalfRegister(Work(1), Lo) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
function intrinsic:or
scoreboard players operation %work%2%lo reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2), Hi), lhs: HalfRegister(Work(0), Hi), op: "|=", rhs: HalfRegister(Work(1), Hi) }
scoreboard players operation %param0%0 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%hi reg
function intrinsic:or
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
# Comment("LocalGet { local_index: 2 }")
# LocalGet { local_index: 2 }
# SetLocalPtr(2)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-3 0 1
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
# Comment("I64Const { value: -1 }")
# I64Const { value: -1 }
# PushI64Const(-1)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value -1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value -1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
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
# Comment("LocalTee { local_index: 2 }")
# LocalTee { local_index: 2 }
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
# Comment("I64Const { value: -1 }")
# I64Const { value: -1 }
# PushI64Const(-1)
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value -1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] run data modify block ~ ~ ~ RecordItem.tag.Memory set value -1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
# Comment("I64Ne")
# I64Ne
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
# I64Eq { dst: Work(2), lhs: Work(0), invert: true, rhs: Work(1) }
execute store success score %work%2%lo reg if score %work%0%lo reg = %work%1%lo reg if score %work%0%hi reg = %work%1%hi reg
execute store success score %work%2%lo reg if score %work%2%lo reg matches 0..0
# PushI32From(Work(2))
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%2%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~2 ~ ~
scoreboard players add %stackptr wasm 2
# Comment("BrIf { relative_depth: 0 }")
# BrIf { relative_depth: 0 }
# PopI32Into(Work(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-2 ~ ~
scoreboard players remove %stackptr wasm 2
execute at @e[tag=stackptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
# BranchIf { t_name: BranchTarget { label: Label { func_idx: CodeFuncIdx(0), idx: 3 }, to_pop: 0, ty: [] }, f_name: BranchTarget { label: Label { func_idx: CodeFuncIdx(0), idx: 4 }, to_pop: 0, ty: [] }, cond: Work(0) }
#   BranchIf { t_name: BranchTarget { label: Label { func_idx: CodeFuncIdx(0), idx: 3 }, to_pop: 0, ty: [] }, f_name: BranchTarget { label: Label { func_idx: CodeFuncIdx(0), idx: 4 }, to_pop: 0, ty: [] }, cond: Work(0) }
scoreboard players set %%taken wasm 0
scoreboard players operation %%tempcond reg = %work%0%lo reg
#   Branch to __wasm0_3
#   Jump to __wasm0_3
execute if score %%taken wasm matches 0 unless score %%tempcond reg matches 0..0 run function intrinsic:i64divrem/__wasm0_3
#   Branch to __wasm0_4
#   Jump to __wasm0_4
execute if score %%taken wasm matches 0 if score %%tempcond reg matches 0..0 run function intrinsic:i64divrem/__wasm0_4
scoreboard players set %%taken wasm 1