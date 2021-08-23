# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# LoadLocalI64(Work(0, 2), 3)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-4 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-4 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Sub")
# I64Sub
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%0%hi%2%temp reg
# SetConst(HalfRegister(Work(0, 2), Lo), 0)
scoreboard players set %work%0%lo%2%temp reg 0
# SetConst(HalfRegister(Work(0, 2), Hi), 0)
scoreboard players set %work%0%hi%2%temp reg 0
# SetConst(HalfRegister(Work(4, 2), Lo), 1)
scoreboard players set %work%4%lo%2%temp reg 1
# SetConst(HalfRegister(Work(4, 2), Hi), 0)
scoreboard players set %work%4%hi%2%temp reg 0
# I32Op { dst: HalfRegister(Work(1, 2), Lo), lhs: HalfRegister(Work(1, 2), Lo), op: "*=", rhs: Const(-1) }
scoreboard players operation %work%1%lo%2%temp reg *= %%-1 reg
# AddI32Const(HalfRegister(Work(1, 2), Lo), -1)
scoreboard players remove %work%1%lo%2%temp reg 1
# I32Op { dst: HalfRegister(Work(1, 2), Hi), lhs: HalfRegister(Work(1, 2), Hi), op: "*=", rhs: Const(-1) }
scoreboard players operation %work%1%hi%2%temp reg *= %%-1 reg
# AddI32Const(HalfRegister(Work(1, 2), Hi), -1)
scoreboard players remove %work%1%hi%2%temp reg 1
# I64Add { dst: Work(5, 2), lhs: Work(1, 2), rhs: Work(4, 2) }
scoreboard players operation %work%5%lo%2%temp reg = %work%1%lo%2%temp reg
scoreboard players operation %work%5%hi%2%temp reg = %work%1%hi%2%temp reg
scoreboard players operation %work%5%lo%2%temp reg += %work%4%lo%2%temp reg
scoreboard players operation %work%5%hi%2%temp reg += %work%4%hi%2%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%1%lo%2%temp reg matches ..-1 if score %work%4%lo%2%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%lo%2%temp reg matches ..-1 if score %work%4%lo%2%temp reg matches 0.. if score %work%5%lo%2%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%lo%2%temp reg matches 0.. if score %work%4%lo%2%temp reg matches ..-1 if score %work%5%lo%2%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%5%hi%2%temp reg += %temp%10%lo reg
# Copy { dst: HalfRegister(Work(1, 2), Lo), src: HalfRegister(Work(5, 2), Lo) }
scoreboard players operation %work%1%lo%2%temp reg = %work%5%lo%2%temp reg
# Copy { dst: HalfRegister(Work(1, 2), Hi), src: HalfRegister(Work(5, 2), Hi) }
scoreboard players operation %work%1%hi%2%temp reg = %work%5%hi%2%temp reg
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
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# LoadLocalI64(Work(0, 2), 0)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-1 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-1 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%2%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%2%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Const { value: -1 }")
# I64Const { value: -1 }
# PushI64Const(-1)
scoreboard players set %stack%3%lo%2%temp reg -1
scoreboard players set %stack%3%hi%2%temp reg -1
# Comment("I64GtS")
# I64GtS
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%3%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%3%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%2%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%2%hi%2%temp reg
# I64SComp { dst: Work(2, 2), lhs: Work(0, 2), op: GreaterThan, rhs: Work(1, 2) }
scoreboard players set %work%2%lo%2%temp reg 0
execute if score %work%1%lo%2%temp reg matches ..-1 if score %work%0%lo%2%temp reg matches 0.. run scoreboard players set %work%2%lo%2%temp reg 0
execute if score %work%1%lo%2%temp reg matches 0.. if score %work%0%lo%2%temp reg matches ..-1 run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%1%lo%2%temp reg matches ..-1 if score %work%0%lo%2%temp reg matches ..-1 if score %work%1%lo%2%temp reg < %work%0%lo%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%1%lo%2%temp reg matches 0.. if score %work%0%lo%2%temp reg matches 0.. if score %work%1%lo%2%temp reg < %work%0%lo%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%1%hi%2%temp reg < %work%0%hi%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%1%hi%2%temp reg > %work%0%hi%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 0
# PushI32From(Work(2, 2))
scoreboard players operation %stack%2%lo%2%temp reg = %work%2%lo%2%temp reg
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# LoadLocalI64(Work(0, 2), 0)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-1 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-1 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%3%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%3%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Const { value: 63 }")
# I64Const { value: 63 }
# PushI64Const(63)
scoreboard players set %stack%4%lo%2%temp reg 63
scoreboard players set %stack%4%hi%2%temp reg 0
# Comment("I64ShrU")
# I64ShrU
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%4%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%4%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%3%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%3%hi%2%temp reg
# I64ShrU { dst: Work(2, 2), lhs: Work(0, 2), rhs: Work(1, 2) }
scoreboard players operation %param0%0 reg = %work%0%lo%2%temp reg
scoreboard players operation %param0%1 reg = %work%0%hi%2%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%2%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:lshr_i64
scoreboard players operation %work%2%lo%2%temp reg = %param0%0 reg
scoreboard players operation %work%2%hi%2%temp reg = %param0%1 reg
# PushI64From(Work(2, 2))
scoreboard players operation %stack%3%lo%2%temp reg = %work%2%lo%2%temp reg
scoreboard players operation %stack%3%hi%2%temp reg = %work%2%hi%2%temp reg
# Comment("I32WrapI64")
# I32WrapI64
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%3%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%3%hi%2%temp reg
# PushI32From(Work(0, 2))
scoreboard players operation %stack%3%lo%2%temp reg = %work%0%lo%2%temp reg
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# LoadLocalI64(Work(0, 2), 1)
execute at @e[tag=frameptr] store result score %work%0%lo%2%temp reg run data get block ~-2 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%2%temp reg run data get block ~-2 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 2))
scoreboard players operation %stack%4%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%4%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%5%lo%2%temp reg 0
scoreboard players set %stack%5%hi%2%temp reg 0
# Comment("I64LtS")
# I64LtS
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%5%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%5%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%4%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%4%hi%2%temp reg
# I64SComp { dst: Work(2, 2), lhs: Work(0, 2), op: LessThan, rhs: Work(1, 2) }
scoreboard players set %work%2%lo%2%temp reg 0
execute if score %work%0%lo%2%temp reg matches ..-1 if score %work%1%lo%2%temp reg matches 0.. run scoreboard players set %work%2%lo%2%temp reg 0
execute if score %work%0%lo%2%temp reg matches 0.. if score %work%1%lo%2%temp reg matches ..-1 run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%0%lo%2%temp reg matches ..-1 if score %work%1%lo%2%temp reg matches ..-1 if score %work%0%lo%2%temp reg < %work%1%lo%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%0%lo%2%temp reg matches 0.. if score %work%1%lo%2%temp reg matches 0.. if score %work%0%lo%2%temp reg < %work%1%lo%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%0%hi%2%temp reg < %work%1%hi%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 1
execute if score %work%0%hi%2%temp reg > %work%1%hi%2%temp reg run scoreboard players set %work%2%lo%2%temp reg 0
# PushI32From(Work(2, 2))
scoreboard players operation %stack%4%lo%2%temp reg = %work%2%lo%2%temp reg
# Comment("Select")
# Select
# PopI32Into(Work(2, 2))
scoreboard players operation %work%2%lo%2%temp reg = %stack%4%lo%2%temp reg
# PopI32Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%3%lo%2%temp reg
# PopI32Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%2%lo%2%temp reg
# SelectI32 { dst_reg: Work(0, 2), true_reg: Work(0, 2), false_reg: Work(1, 2), cond_reg: Work(2, 2) }
scoreboard players operation %work%0%lo%2%temp reg = %work%0%lo%2%temp reg
execute if score %work%2%lo%2%temp reg matches 0..0 run scoreboard players operation %work%0%lo%2%temp reg = %work%1%lo%2%temp reg
# PushI32From(Work(0, 2))
scoreboard players operation %stack%2%lo%2%temp reg = %work%0%lo%2%temp reg
# Comment("Select")
# Select
# PopI32Into(Work(2, 2))
scoreboard players operation %work%2%lo%2%temp reg = %stack%2%lo%2%temp reg
# PopI64Into(Work(1, 2))
scoreboard players operation %work%1%lo%2%temp reg = %stack%1%lo%2%temp reg
scoreboard players operation %work%1%hi%2%temp reg = %stack%1%hi%2%temp reg
# PopI64Into(Work(0, 2))
scoreboard players operation %work%0%lo%2%temp reg = %stack%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %stack%0%hi%2%temp reg
# SelectI64 { dst_reg: Work(0, 2), true_reg: Work(0, 2), false_reg: Work(1, 2), cond_reg: Work(2, 2) }
scoreboard players operation %work%0%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %work%0%hi%2%temp reg = %work%0%hi%2%temp reg
execute if score %work%2%lo%2%temp reg matches 0..0 run scoreboard players operation %work%0%lo%2%temp reg = %work%1%lo%2%temp reg
execute if score %work%2%lo%2%temp reg matches 0..0 run scoreboard players operation %work%0%hi%2%temp reg = %work%1%hi%2%temp reg
# PushI64From(Work(0, 2))
scoreboard players operation %stack%0%lo%2%temp reg = %work%0%lo%2%temp reg
scoreboard players operation %stack%0%hi%2%temp reg = %work%0%hi%2%temp reg
# Comment("End")
# End
#   Branch to __wasm2_1
scoreboard players operation %return%0%lo reg = %stack%0%lo%2%temp reg
scoreboard players operation %return%0%hi reg = %stack%0%hi%2%temp reg
scoreboard players operation %stack%0%lo%2%temp reg = %return%0%lo reg
scoreboard players operation %stack%0%hi%2%temp reg = %return%0%hi reg
#   Jump to __wasm2_1
function intrinsic:i64divrem/__wasm2_1
scoreboard players set %%taken wasm 1