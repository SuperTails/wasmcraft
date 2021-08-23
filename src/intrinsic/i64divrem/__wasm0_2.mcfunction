# Comment("LocalGet { local_index: 4 }")
# LocalGet { local_index: 4 }
# LoadLocalI64(Work(0, 0), 4)
execute at @e[tag=frameptr] store result score %work%0%lo%0%temp reg run data get block ~-5 ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=frameptr] store result score %work%0%hi%0%temp reg run data get block ~-5 ~ ~1 RecordItem.tag.Memory 1
# PushI64From(Work(0, 0))
scoreboard players operation %stack%0%lo%0%temp reg = %work%0%lo%0%temp reg
scoreboard players operation %stack%0%hi%0%temp reg = %work%0%hi%0%temp reg
# Comment("End")
# End
#   Branch to __wasm0_1
scoreboard players operation %return%0%lo reg = %stack%0%lo%0%temp reg
scoreboard players operation %return%0%hi reg = %stack%0%hi%0%temp reg
scoreboard players operation %stack%0%lo%0%temp reg = %return%0%lo reg
scoreboard players operation %stack%0%hi%0%temp reg = %return%0%hi reg
#   Jump to __wasm0_1
function intrinsic:i64divrem/__wasm0_1
scoreboard players set %%taken wasm 1