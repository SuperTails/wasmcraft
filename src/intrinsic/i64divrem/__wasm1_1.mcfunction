# Comment(" Pop frame")
#  Pop frame
# PopFrame(4)
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~-4 ~ ~
execute at @e[tag=frameptr] run fill ~ ~ ~ ~3 ~ ~1 minecraft:air
# Comment(" Save return values")
#  Save return values
# PopI64Into(Return(0))
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %return%0%hi reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=stackptr] as @e[tag=stackptr] run tp @s ~-1 ~ ~
scoreboard players remove %stackptr wasm 1
execute at @e[tag=stackptr] store result score %return%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
scoreboard players set %%taken wasm 1