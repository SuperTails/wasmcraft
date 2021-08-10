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
# Comment("End")
# End
# Branch(BranchTarget { label: Label { func_idx: CodeFuncIdx(1), idx: 1 }, to_pop: 0, ty: [I64] })
#   Branch to __wasm1_1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %return%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %return%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %return%0%lo reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
execute at @e[tag=stackptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %return%0%hi reg
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~1 ~ ~
scoreboard players add %stackptr wasm 1
#   Jump to __wasm1_1
function intrinsic:i64divrem/__wasm1_1
scoreboard players set %%taken wasm 1