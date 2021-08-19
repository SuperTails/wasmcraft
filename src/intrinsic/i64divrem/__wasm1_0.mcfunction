# PushFrame(4)
# Push frame with 4 locals
execute at @e[tag=frameptr] run fill ~ ~ ~ ~3 ~ ~1 minecraft:jukebox{RecordItem:{id:"minecraft:stone",Count:1b,tag:{Memory:0}}}
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~4 ~ ~
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
scoreboard players set %stack%0%lo reg 63
scoreboard players set %stack%0%hi reg 0
# Comment("LocalSet { local_index: 2 }")
# LocalSet { local_index: 2 }
# SetLocalPtr(2)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-3 0 1
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%0%lo reg
scoreboard players operation %work%0%hi reg = %stack%0%hi reg
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("I64Const { value: 0 }")
# I64Const { value: 0 }
# PushI64Const(0)
scoreboard players set %stack%0%lo reg 0
scoreboard players set %stack%0%hi reg 0
# Comment("LocalSet { local_index: 3 }")
# LocalSet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# PopI64Into(Work(0))
scoreboard players operation %work%0%lo reg = %stack%0%lo reg
scoreboard players operation %work%0%hi reg = %stack%0%hi reg
# StoreLocalI64(Work(0))
execute at @e[tag=localptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %work%0%lo reg
execute at @e[tag=localptr] store result block ~ ~ ~1 RecordItem.tag.Memory int 1 run scoreboard players get %work%0%hi reg
# Comment("Loop { ty: Type(EmptyBlockType) }")
# Loop { ty: Type(EmptyBlockType) }
#   Branch to __wasm1_3
#   Jump to __wasm1_3
function intrinsic:i64divrem/__wasm1_3
scoreboard players set %%taken wasm 1