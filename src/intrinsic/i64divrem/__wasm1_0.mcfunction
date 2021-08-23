# PushFrame(4)
# Push frame with 4 locals
execute at @e[tag=frameptr] run fill ~ ~ ~ ~3 ~ ~1 minecraft:jukebox{RecordItem:{id:"minecraft:stone",Count:1b,tag:{Memory:0}}}
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~4 ~ ~
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
# SetConst(HalfRegister(Work(0, 1), Lo), 63)
scoreboard players set %work%0%lo%1%temp reg 63
# SetConst(HalfRegister(Work(0, 1), Hi), 0)
scoreboard players set %work%0%hi%1%temp reg 0
# StoreLocalI64(Work(0, 1), 2)
execute at @e[tag=frameptr] store result block ~-3 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%1%temp reg
execute at @e[tag=frameptr] store result block ~-3 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%1%temp reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# SetConst(HalfRegister(Work(0, 1), Lo), 0)
scoreboard players set %work%0%lo%1%temp reg 0
# SetConst(HalfRegister(Work(0, 1), Hi), 0)
scoreboard players set %work%0%hi%1%temp reg 0
# StoreLocalI64(Work(0, 1), 3)
execute at @e[tag=frameptr] store result block ~-4 ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo%1%temp reg
execute at @e[tag=frameptr] store result block ~-4 ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi%1%temp reg
# Comment("Loop { ty: Type(EmptyBlockType) }")
# Loop { ty: Type(EmptyBlockType) }
#   Branch to __wasm1_3
#   Jump to __wasm1_3
function intrinsic:i64divrem/__wasm1_3
scoreboard players set %%taken wasm 1