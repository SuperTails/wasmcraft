scoreboard players operation %%ptr reg = %ptr reg
scoreboard players operation %%ptr reg /= %%4 reg
scoreboard players operation %z reg = %%ptr reg 
scoreboard players operation %z reg %= %%8 reg
scoreboard players operation %%ptr reg /= %%8 reg
scoreboard players operation %y reg = %%ptr reg
scoreboard players operation %y reg %= %%256 reg
scoreboard players operation %%ptr reg /= %%256 reg
scoreboard players operation %x reg = %%ptr reg
execute as @e[tag=memoryptr] store result entity @s Pos[0] double 1 run scoreboard players get %x reg
execute as @e[tag=memoryptr] store result entity @s Pos[1] double 1 run scoreboard players get %y reg
execute as @e[tag=memoryptr] store result entity @s Pos[2] double 1 run scoreboard players get %z reg
execute as @e[tag=memoryptr] at @e[tag=memoryptr] run tp @s ~ ~ ~8