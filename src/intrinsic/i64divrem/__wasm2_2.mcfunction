# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%0%lo reg 0
scoreboard players set %stack%0%hi reg 0
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
scoreboard players operation %stack%1%lo reg = %work%0%lo reg
scoreboard players operation %stack%1%hi reg = %work%0%hi reg
# Comment("I64Sub")
# I64Sub
# PopI64Into(Work(1))
scoreboard players operation %work%1%lo reg = %stack%1%lo reg
scoreboard players operation %work%1%hi reg = %stack%1%hi reg
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%0%lo reg
scoreboard players operation %work%0%hi reg = %stack%0%hi reg
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
scoreboard players operation %stack%0%lo reg = %work%2%lo reg
scoreboard players operation %stack%0%hi reg = %work%2%hi reg
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
scoreboard players operation %stack%1%lo reg = %work%0%lo reg
scoreboard players operation %stack%1%hi reg = %work%0%hi reg
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# SetLocalPtr(0)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-1 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
scoreboard players operation %stack%2%lo reg = %work%0%lo reg
scoreboard players operation %stack%2%hi reg = %work%0%hi reg
# Comment("I64Const { value: -1 }")
# I64Const { value: -1 }
# PushI64Const(-1)
scoreboard players set %stack%3%lo reg -1
scoreboard players set %stack%3%hi reg -1
# Comment("I64GtS")
# I64GtS
# PopI64Into(Work(1))
scoreboard players operation %work%1%lo reg = %stack%3%lo reg
scoreboard players operation %work%1%hi reg = %stack%3%hi reg
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%2%lo reg
scoreboard players operation %work%0%hi reg = %stack%2%hi reg
# I64SComp { dst: Work(2), lhs: Work(0), op: GreaterThan, rhs: Work(1) }
scoreboard players set %work%2%lo reg 0
execute if score %work%1%lo reg matches ..-1 if score %work%0%lo reg matches 0.. run scoreboard players set %work%2%lo reg 0
execute if score %work%1%lo reg matches 0.. if score %work%0%lo reg matches ..-1 run scoreboard players set %work%2%lo reg 1
execute if score %work%1%lo reg matches ..-1 if score %work%0%lo reg matches ..-1 if score %work%1%lo reg < %work%0%lo reg run scoreboard players set %work%2%lo reg 1
execute if score %work%1%lo reg matches 0.. if score %work%0%lo reg matches 0.. if score %work%1%lo reg < %work%0%lo reg run scoreboard players set %work%2%lo reg 1
execute if score %work%1%hi reg < %work%0%hi reg run scoreboard players set %work%2%lo reg 1
execute if score %work%1%hi reg > %work%0%hi reg run scoreboard players set %work%2%lo reg 0
# PushI32From(Work(2))
scoreboard players operation %stack%2%lo reg = %work%2%lo reg
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# SetLocalPtr(0)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-1 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
scoreboard players operation %stack%3%lo reg = %work%0%lo reg
scoreboard players operation %stack%3%hi reg = %work%0%hi reg
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# PushI64Const(63)
scoreboard players set %stack%4%lo reg 63
scoreboard players set %stack%4%hi reg 0
# Comment("I64ShrU")
# I64ShrU
# PopI64Into(Work(1))
scoreboard players operation %work%1%lo reg = %stack%4%lo reg
scoreboard players operation %work%1%hi reg = %stack%4%hi reg
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%3%lo reg
scoreboard players operation %work%0%hi reg = %stack%3%hi reg
# I64ShrU { dst: Work(2), lhs: Work(0), rhs: Work(1) }
scoreboard players operation %param0%0 reg = %work%0%lo reg
scoreboard players operation %param0%1 reg = %work%0%hi reg
scoreboard players operation %param1%0 reg = %work%1%lo reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:lshr_i64
scoreboard players operation %work%2%lo reg = %param0%0 reg
scoreboard players operation %work%2%hi reg = %param0%1 reg
# PushI64From(Work(2))
scoreboard players operation %stack%3%lo reg = %work%2%lo reg
scoreboard players operation %stack%3%hi reg = %work%2%hi reg
# Comment("I32WrapI64")
# I32WrapI64
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%3%lo reg
scoreboard players operation %work%0%hi reg = %stack%3%hi reg
# PushI32From(Work(0))
scoreboard players operation %stack%3%lo reg = %work%0%lo reg
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# SetLocalPtr(1)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-2 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
scoreboard players operation %stack%4%lo reg = %work%0%lo reg
scoreboard players operation %stack%4%hi reg = %work%0%hi reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%5%lo reg 0
scoreboard players set %stack%5%hi reg 0
# Comment("I64LtS")
# I64LtS
# PopI64Into(Work(1))
scoreboard players operation %work%1%lo reg = %stack%5%lo reg
scoreboard players operation %work%1%hi reg = %stack%5%hi reg
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%4%lo reg
scoreboard players operation %work%0%hi reg = %stack%4%hi reg
# I64SComp { dst: Work(2), lhs: Work(0), op: LessThan, rhs: Work(1) }
scoreboard players set %work%2%lo reg 0
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches 0.. run scoreboard players set %work%2%lo reg 0
execute if score %work%0%lo reg matches 0.. if score %work%1%lo reg matches ..-1 run scoreboard players set %work%2%lo reg 1
execute if score %work%0%lo reg matches ..-1 if score %work%1%lo reg matches ..-1 if score %work%0%lo reg < %work%1%lo reg run scoreboard players set %work%2%lo reg 1
execute if score %work%0%lo reg matches 0.. if score %work%1%lo reg matches 0.. if score %work%0%lo reg < %work%1%lo reg run scoreboard players set %work%2%lo reg 1
execute if score %work%0%hi reg < %work%1%hi reg run scoreboard players set %work%2%lo reg 1
execute if score %work%0%hi reg > %work%1%hi reg run scoreboard players set %work%2%lo reg 0
# PushI32From(Work(2))
scoreboard players operation %stack%4%lo reg = %work%2%lo reg
# Comment("Select")
# Select
# PopI32Into(Work(2))
scoreboard players operation %work%2%lo reg = %stack%4%lo reg
# PopI32Into(Work(1))
scoreboard players operation %work%1%lo reg = %stack%3%lo reg
# PopI32Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%2%lo reg
# SelectI32 { dst_reg: Work(0), true_reg: Work(0), false_reg: Work(1), cond_reg: Work(2) }
scoreboard players operation %work%0%lo reg = %work%0%lo reg
execute if score %work%2%lo reg matches 0..0 run scoreboard players operation %work%0%lo reg = %work%1%lo reg
# PushI32From(Work(0))
scoreboard players operation %stack%2%lo reg = %work%0%lo reg
# Comment("Select")
# Select
# PopI32Into(Work(2))
scoreboard players operation %work%2%lo reg = %stack%2%lo reg
# PopI64Into(Work(1))
scoreboard players operation %work%1%lo reg = %stack%1%lo reg
scoreboard players operation %work%1%hi reg = %stack%1%hi reg
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%0%lo reg
scoreboard players operation %work%0%hi reg = %stack%0%hi reg
# SelectI64 { dst_reg: Work(0), true_reg: Work(0), false_reg: Work(1), cond_reg: Work(2) }
scoreboard players operation %work%0%lo reg = %work%0%lo reg
scoreboard players operation %work%0%hi reg = %work%0%hi reg
execute if score %work%2%lo reg matches 0..0 run scoreboard players operation %work%0%lo reg = %work%1%lo reg
execute if score %work%2%lo reg matches 0..0 run scoreboard players operation %work%0%hi reg = %work%1%hi reg
# PushI64From(Work(0))
scoreboard players operation %stack%0%lo reg = %work%0%lo reg
scoreboard players operation %stack%0%hi reg = %work%0%hi reg
# Comment("End")
# End
#   Branch to __wasm2_1
scoreboard players operation %return%0%lo reg = %stack%0%lo reg
scoreboard players operation %return%0%hi reg = %stack%0%hi reg
scoreboard players operation %stack%0%lo reg = %return%0%lo reg
scoreboard players operation %stack%0%hi reg = %return%0%hi reg
#   Jump to __wasm2_1
function intrinsic:i64divrem/__wasm2_1
scoreboard players set %%taken wasm 1