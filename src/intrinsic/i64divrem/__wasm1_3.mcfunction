# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# LoadLocalI64(Work(0, 1), 3)
execute at @e[tag=frameptr] store result score %work%0%lo%1%temp reg run data get block ~-4 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%1%temp reg run data get block ~-4 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("I64Const { value: 1 }")
# I64Const { value: 1 }
# Comment("I64Shl")
# I64Shl
# SetConst(HalfRegister(Work(1, 1), Lo), 1)
scoreboard players set %work%1%lo%1%temp reg 1
# SetConst(HalfRegister(Work(1, 1), Hi), 0)
scoreboard players set %work%1%hi%1%temp reg 0
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# I64Shl { dst: Work(2, 1), lhs: Work(0, 1), rhs: Work(1, 1) }
scoreboard players operation %param0%0 reg = %work%0%lo%1%temp reg
scoreboard players operation %param0%1 reg = %work%0%hi%1%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%1%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:shl_64
scoreboard players operation %work%2%lo%1%temp reg = %param0%0 reg
scoreboard players operation %work%2%hi%1%temp reg = %param0%1 reg
# PushI64From(Work(2, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%2%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%2%hi%1%temp reg
# Comment("LocalGet { local_index: 0 }")
# LocalGet { local_index: 0 }
# LoadLocalI64(Work(0, 1), 0)
execute at @e[tag=frameptr] store result score %work%0%lo%1%temp reg run data get block ~-1 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%1%temp reg run data get block ~-1 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 1))
scoreboard players operation %stack%1%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%1%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("LocalGet { local_index: 2 }")
# LocalGet { local_index: 2 }
# LoadLocalI64(Work(0, 1), 2)
execute at @e[tag=frameptr] store result score %work%0%lo%1%temp reg run data get block ~-3 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%1%temp reg run data get block ~-3 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 1))
scoreboard players operation %stack%2%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%2%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("I64ShrU")
# I64ShrU
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%2%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%2%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%1%hi%1%temp reg
# I64ShrU { dst: Work(2, 1), lhs: Work(0, 1), rhs: Work(1, 1) }
scoreboard players operation %param0%0 reg = %work%0%lo%1%temp reg
scoreboard players operation %param0%1 reg = %work%0%hi%1%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%1%temp reg
scoreboard players operation %param1%0 reg %= %%64 reg
function intrinsic:lshr_i64
scoreboard players operation %work%2%lo%1%temp reg = %param0%0 reg
scoreboard players operation %work%2%hi%1%temp reg = %param0%1 reg
# PushI64From(Work(2, 1))
scoreboard players operation %stack%1%lo%1%temp reg = %work%2%lo%1%temp reg
scoreboard players operation %stack%1%hi%1%temp reg = %work%2%hi%1%temp reg
# Comment("I64Const { value: 1 }")
# I64Const { value: 1 }
# PushI64Const(1)
scoreboard players set %stack%2%lo%1%temp reg 1
scoreboard players set %stack%2%hi%1%temp reg 0
# Comment("I64And")
# I64And
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%2%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%2%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%1%hi%1%temp reg
# I32Op { dst: HalfRegister(Work(2, 1), Lo), lhs: HalfRegister(Work(0, 1), Lo), op: "&=", rhs: Reg(HalfRegister(Work(1, 1), Lo)) }
scoreboard players operation %param0%0 reg = %work%0%lo%1%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%1%temp reg
function intrinsic:and
scoreboard players operation %work%2%lo%1%temp reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2, 1), Hi), lhs: HalfRegister(Work(0, 1), Hi), op: "&=", rhs: Reg(HalfRegister(Work(1, 1), Hi)) }
scoreboard players operation %param0%0 reg = %work%0%hi%1%temp reg
scoreboard players operation %param1%0 reg = %work%1%hi%1%temp reg
function intrinsic:and
scoreboard players operation %work%2%hi%1%temp reg = %return%0 reg
# PushI64From(Work(2, 1))
scoreboard players operation %stack%1%lo%1%temp reg = %work%2%lo%1%temp reg
scoreboard players operation %stack%1%hi%1%temp reg = %work%2%hi%1%temp reg
# Comment("I64Or")
# I64Or
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%1%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# I32Op { dst: HalfRegister(Work(2, 1), Lo), lhs: HalfRegister(Work(0, 1), Lo), op: "|=", rhs: Reg(HalfRegister(Work(1, 1), Lo)) }
scoreboard players operation %param0%0 reg = %work%0%lo%1%temp reg
scoreboard players operation %param1%0 reg = %work%1%lo%1%temp reg
function intrinsic:or
scoreboard players operation %work%2%lo%1%temp reg = %return%0 reg
# I32Op { dst: HalfRegister(Work(2, 1), Hi), lhs: HalfRegister(Work(0, 1), Hi), op: "|=", rhs: Reg(HalfRegister(Work(1, 1), Hi)) }
scoreboard players operation %param0%0 reg = %work%0%hi%1%temp reg
scoreboard players operation %param1%0 reg = %work%1%hi%1%temp reg
function intrinsic:or
scoreboard players operation %work%2%hi%1%temp reg = %return%0 reg
# PushI64From(Work(2, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%2%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%2%hi%1%temp reg
# Comment("LocalTee { local_index: 3 }")
# LocalTee { local_index: 3 }
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# PushI64From(Work(0, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%0%hi%1%temp reg
# PushI64From(Work(0, 1))
scoreboard players operation %stack%1%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%1%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%1%hi%1%temp reg
# StoreLocalI64(Work(0, 1), 3)
execute at @e[tag=frameptr] store result block ~-4 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%1%temp reg
execute at @e[tag=frameptr] store result block ~-4 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%1%temp reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%1%lo%1%temp reg 0
scoreboard players set %stack%1%hi%1%temp reg 0
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# LoadLocalI64(Work(0, 1), 1)
execute at @e[tag=frameptr] store result score %work%0%lo%1%temp reg run data get block ~-2 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%1%temp reg run data get block ~-2 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 1))
scoreboard players operation %stack%2%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%2%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# LoadLocalI64(Work(0, 1), 3)
execute at @e[tag=frameptr] store result score %work%0%lo%1%temp reg run data get block ~-4 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%1%temp reg run data get block ~-4 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 1))
scoreboard players operation %stack%3%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%3%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("LocalGet { local_index: 1 }")
# LocalGet { local_index: 1 }
# LoadLocalI64(Work(0, 1), 1)
execute at @e[tag=frameptr] store result score %work%0%lo%1%temp reg run data get block ~-2 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%1%temp reg run data get block ~-2 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 1))
scoreboard players operation %stack%4%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%4%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("I64LtU")
# I64LtU
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%4%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%4%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%3%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%3%hi%1%temp reg
# I64UComp { dst: Work(2, 1), lhs: Work(0, 1), op: LessThan, rhs: Work(1, 1) }
scoreboard players set %temp%3%lo reg 0
execute if score %work%0%hi%1%temp reg matches ..-1 if score %work%1%hi%1%temp reg matches 0.. run scoreboard players set %temp%3%lo reg 0
execute if score %work%0%hi%1%temp reg matches 0.. if score %work%1%hi%1%temp reg matches ..-1 run scoreboard players set %temp%3%lo reg 1
execute if score %work%0%hi%1%temp reg matches ..-1 if score %work%1%hi%1%temp reg matches ..-1 if score %work%0%hi%1%temp reg < %work%1%hi%1%temp reg run scoreboard players set %temp%3%lo reg 1
execute if score %work%0%hi%1%temp reg matches 0.. if score %work%1%hi%1%temp reg matches 0.. if score %work%0%hi%1%temp reg < %work%1%hi%1%temp reg run scoreboard players set %temp%3%lo reg 1
scoreboard players set %temp%4%lo reg 0
execute if score %work%1%hi%1%temp reg matches ..-1 if score %work%0%hi%1%temp reg matches 0.. run scoreboard players set %temp%4%lo reg 0
execute if score %work%1%hi%1%temp reg matches 0.. if score %work%0%hi%1%temp reg matches ..-1 run scoreboard players set %temp%4%lo reg 1
execute if score %work%1%hi%1%temp reg matches ..-1 if score %work%0%hi%1%temp reg matches ..-1 if score %work%1%hi%1%temp reg < %work%0%hi%1%temp reg run scoreboard players set %temp%4%lo reg 1
execute if score %work%1%hi%1%temp reg matches 0.. if score %work%0%hi%1%temp reg matches 0.. if score %work%1%hi%1%temp reg < %work%0%hi%1%temp reg run scoreboard players set %temp%4%lo reg 1
execute store success score %temp%5%lo reg if score %work%0%hi%1%temp reg = %work%1%hi%1%temp reg
scoreboard players set %temp%6%lo reg 0
execute if score %work%0%lo%1%temp reg matches ..-1 if score %work%1%lo%1%temp reg matches 0.. run scoreboard players set %temp%6%lo reg 0
execute if score %work%0%lo%1%temp reg matches 0.. if score %work%1%lo%1%temp reg matches ..-1 run scoreboard players set %temp%6%lo reg 1
execute if score %work%0%lo%1%temp reg matches ..-1 if score %work%1%lo%1%temp reg matches ..-1 if score %work%0%lo%1%temp reg < %work%1%lo%1%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %work%0%lo%1%temp reg matches 0.. if score %work%1%lo%1%temp reg matches 0.. if score %work%0%lo%1%temp reg < %work%1%lo%1%temp reg run scoreboard players set %temp%6%lo reg 1
execute if score %temp%3%lo reg matches 1.. run scoreboard players set %work%2%lo%1%temp reg 1
execute if score %temp%4%lo reg matches 1.. run scoreboard players set %work%2%lo%1%temp reg 0
execute if score %temp%5%lo reg matches 1.. run scoreboard players operation %work%2%lo%1%temp reg = %temp%6%lo reg
# PushI32From(Work(2, 1))
scoreboard players operation %stack%3%lo%1%temp reg = %work%2%lo%1%temp reg
# Comment("Select")
# Select
# PopI32Into(Work(2, 1))
scoreboard players operation %work%2%lo%1%temp reg = %stack%3%lo%1%temp reg
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%2%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%2%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%1%hi%1%temp reg
# SelectI64 { dst_reg: Work(0, 1), true_reg: Work(0, 1), false_reg: Work(1, 1), cond_reg: Work(2, 1) }
scoreboard players operation %work%0%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %work%0%hi%1%temp reg
execute if score %work%2%lo%1%temp reg matches 0..0 run scoreboard players operation %work%0%lo%1%temp reg = %work%1%lo%1%temp reg
execute if score %work%2%lo%1%temp reg matches 0..0 run scoreboard players operation %work%0%hi%1%temp reg = %work%1%hi%1%temp reg
# PushI64From(Work(0, 1))
scoreboard players operation %stack%1%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%1%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("I64Sub")
# I64Sub
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%1%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# SetConst(HalfRegister(Work(4, 1), Lo), 1)
scoreboard players set %work%4%lo%1%temp reg 1
# SetConst(HalfRegister(Work(4, 1), Hi), 0)
scoreboard players set %work%4%hi%1%temp reg 0
# I32Op { dst: HalfRegister(Work(1, 1), Lo), lhs: HalfRegister(Work(1, 1), Lo), op: "*=", rhs: Const(-1) }
scoreboard players operation %work%1%lo%1%temp reg *= %%-1 reg
# AddI32Const(HalfRegister(Work(1, 1), Lo), -1)
scoreboard players remove %work%1%lo%1%temp reg 1
# I32Op { dst: HalfRegister(Work(1, 1), Hi), lhs: HalfRegister(Work(1, 1), Hi), op: "*=", rhs: Const(-1) }
scoreboard players operation %work%1%hi%1%temp reg *= %%-1 reg
# AddI32Const(HalfRegister(Work(1, 1), Hi), -1)
scoreboard players remove %work%1%hi%1%temp reg 1
# I64Add { dst: Work(5, 1), lhs: Work(1, 1), rhs: Work(4, 1) }
scoreboard players operation %work%5%lo%1%temp reg = %work%1%lo%1%temp reg
scoreboard players operation %work%5%hi%1%temp reg = %work%1%hi%1%temp reg
scoreboard players operation %work%5%lo%1%temp reg += %work%4%lo%1%temp reg
scoreboard players operation %work%5%hi%1%temp reg += %work%4%hi%1%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%1%lo%1%temp reg matches ..-1 if score %work%4%lo%1%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%lo%1%temp reg matches ..-1 if score %work%4%lo%1%temp reg matches 0.. if score %work%5%lo%1%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%1%lo%1%temp reg matches 0.. if score %work%4%lo%1%temp reg matches ..-1 if score %work%5%lo%1%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%5%hi%1%temp reg += %temp%10%lo reg
# Copy { dst: HalfRegister(Work(1, 1), Lo), src: HalfRegister(Work(5, 1), Lo) }
scoreboard players operation %work%1%lo%1%temp reg = %work%5%lo%1%temp reg
# Copy { dst: HalfRegister(Work(1, 1), Hi), src: HalfRegister(Work(5, 1), Hi) }
scoreboard players operation %work%1%hi%1%temp reg = %work%5%hi%1%temp reg
# I64Add { dst: Work(2, 1), lhs: Work(0, 1), rhs: Work(1, 1) }
scoreboard players operation %work%2%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %work%2%hi%1%temp reg = %work%0%hi%1%temp reg
scoreboard players operation %work%2%lo%1%temp reg += %work%1%lo%1%temp reg
scoreboard players operation %work%2%hi%1%temp reg += %work%1%hi%1%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%0%lo%1%temp reg matches ..-1 if score %work%1%lo%1%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%1%temp reg matches ..-1 if score %work%1%lo%1%temp reg matches 0.. if score %work%2%lo%1%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%1%temp reg matches 0.. if score %work%1%lo%1%temp reg matches ..-1 if score %work%2%lo%1%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%hi%1%temp reg += %temp%10%lo reg
# PushI64From(Work(2, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%2%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%2%hi%1%temp reg
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# StoreLocalI64(Work(0, 1), 3)
execute at @e[tag=frameptr] store result block ~-4 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%1%temp reg
execute at @e[tag=frameptr] store result block ~-4 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%1%temp reg
# Comment("LocalGet { local_index: 2 }")
# LocalGet { local_index: 2 }
# LoadLocalI64(Work(0, 1), 2)
execute at @e[tag=frameptr] store result score %work%0%lo%1%temp reg run data get block ~-3 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%1%temp reg run data get block ~-3 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("I64Const { value: -1 }")
# I64Const { value: -1 }
# PushI64Const(-1)
scoreboard players set %stack%1%lo%1%temp reg -1
scoreboard players set %stack%1%hi%1%temp reg -1
# Comment("I64Add")
# I64Add
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%1%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# I64Add { dst: Work(2, 1), lhs: Work(0, 1), rhs: Work(1, 1) }
scoreboard players operation %work%2%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %work%2%hi%1%temp reg = %work%0%hi%1%temp reg
scoreboard players operation %work%2%lo%1%temp reg += %work%1%lo%1%temp reg
scoreboard players operation %work%2%hi%1%temp reg += %work%1%hi%1%temp reg
scoreboard players set %temp%10%lo reg 0
execute if score %work%0%lo%1%temp reg matches ..-1 if score %work%1%lo%1%temp reg matches ..-1 run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%1%temp reg matches ..-1 if score %work%1%lo%1%temp reg matches 0.. if score %work%2%lo%1%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
execute if score %work%0%lo%1%temp reg matches 0.. if score %work%1%lo%1%temp reg matches ..-1 if score %work%2%lo%1%temp reg matches 0.. run scoreboard players set %temp%10%lo reg 1
scoreboard players operation %work%2%hi%1%temp reg += %temp%10%lo reg
# PushI64From(Work(2, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%2%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%2%hi%1%temp reg
# Comment("LocalTee { local_index: 2 }")
# LocalTee { local_index: 2 }
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# PushI64From(Work(0, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%0%hi%1%temp reg = %work%0%hi%1%temp reg
# PushI64From(Work(0, 1))
scoreboard players operation %stack%1%lo%1%temp reg = %work%0%lo%1%temp reg
scoreboard players operation %stack%1%hi%1%temp reg = %work%0%hi%1%temp reg
# Comment("LocalSet { local_index: 2 }")
# LocalSet { local_index: 2 }
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%1%hi%1%temp reg
# StoreLocalI64(Work(0, 1), 2)
execute at @e[tag=frameptr] store result block ~-3 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%1%temp reg
execute at @e[tag=frameptr] store result block ~-3 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%1%temp reg
# Comment("I64Const { value: -1 }")
# I64Const { value: -1 }
# PushI64Const(-1)
scoreboard players set %stack%1%lo%1%temp reg -1
scoreboard players set %stack%1%hi%1%temp reg -1
# Comment("I64Ne")
# I64Ne
# PopI64Into(Work(1, 1))
scoreboard players operation %work%1%lo%1%temp reg = %stack%1%lo%1%temp reg
scoreboard players operation %work%1%hi%1%temp reg = %stack%1%hi%1%temp reg
# PopI64Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players operation %work%0%hi%1%temp reg = %stack%0%hi%1%temp reg
# I64Eq { dst: Work(2, 1), lhs: Work(0, 1), invert: true, rhs: Work(1, 1) }
scoreboard players set %work%2%lo%1%temp reg 1
execute unless score %work%0%lo%1%temp reg = %work%1%lo%1%temp reg run scoreboard players set %work%2%lo%1%temp reg 0
execute unless score %work%0%hi%1%temp reg = %work%1%hi%1%temp reg run scoreboard players set %work%2%lo%1%temp reg 0
execute store success score %work%2%lo%1%temp reg if score %work%2%lo%1%temp reg matches 0..0
# PushI32From(Work(2, 1))
scoreboard players operation %stack%0%lo%1%temp reg = %work%2%lo%1%temp reg
# Comment("BrIf { relative_depth: 0 }")
# BrIf { relative_depth: 0 }
# PopI32Into(Work(0, 1))
scoreboard players operation %work%0%lo%1%temp reg = %stack%0%lo%1%temp reg
scoreboard players set %%taken wasm 0
scoreboard players operation %%tempcond reg = %work%0%lo%1%temp reg
#   Branch to __wasm1_3
#   Jump to __wasm1_3
execute if score %%taken wasm matches 0 unless score %%tempcond reg matches 0..0 run function intrinsic:i64divrem/__wasm1_3
#   Branch to __wasm1_4
#   Jump to __wasm1_4
execute if score %%taken wasm matches 0 if score %%tempcond reg matches 0..0 run function intrinsic:i64divrem/__wasm1_4
scoreboard players set %%taken wasm 1