# Comment("LocalGet { local_index: 3 }")
# LocalGet { local_index: 3 }
# SetLocalPtr(3)
execute at @e[tag=frameptr] as @e[tag=localptr] run tp @s ~-4 0 1
# LoadLocalI64(Work(0))
execute at @e[tag=localptr] store result score %work%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=localptr] store result score %work%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0))
scoreboard players operation %stack%0%lo reg = %work%0%lo reg
scoreboard players operation %stack%0%hi reg = %work%0%hi reg
# Comment("End")
# End
#   Branch to __wasm1_1
scoreboard players operation %return%0%lo reg = %stack%0%lo reg
scoreboard players operation %return%0%hi reg = %stack%0%hi reg
scoreboard players operation %stack%0%lo reg = %return%0%lo reg
scoreboard players operation %stack%0%hi reg = %return%0%hi reg
#   Jump to __wasm1_1
function intrinsic:i64divrem/__wasm1_1
scoreboard players set %%taken wasm 1