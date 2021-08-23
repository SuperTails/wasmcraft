# Comment(" Pop frame")
#  Pop frame
# PopFrame(9)
execute as @e[tag=frameptr] at @e[tag=frameptr] run tp @s ~-9 ~ ~
execute at @e[tag=frameptr] run fill ~ ~ ~ ~8 ~ ~1 minecraft:air
# Comment(" Save return values")
#  Save return values
# PopI64Into(Return(0))
scoreboard players operation %return%0%lo reg = %stack%0%lo%3%temp reg
scoreboard players operation %return%0%hi reg = %stack%0%hi%3%temp reg
scoreboard players set %%taken wasm 1