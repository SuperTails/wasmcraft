# Comment(" Pop frame")
#  Pop frame
# PopFrame(4)
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~-4 ~ ~
execute at @e[tag=frameptr] run fill ~ ~ ~ ~3 ~ ~1 minecraft:air
# Comment(" Save return values")
#  Save return values
# PopI64Into(Return(0))
scoreboard players operation %return%0%lo reg = %stack%0%lo%1%temp reg
scoreboard players operation %return%0%hi reg = %stack%0%hi%1%temp reg
scoreboard players set %%taken wasm 1