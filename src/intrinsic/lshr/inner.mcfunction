execute store success score %%temp0_lshr_inner reg if score %param0%0 reg matches ..-1

# Have to split this in two because you can't actually subtract i32::MAX
execute if score %%temp0_lshr_inner reg matches 1..1 run scoreboard players remove %param0%0 reg 2147483647
execute if score %%temp0_lshr_inner reg matches 1..1 run scoreboard players remove %param0%0 reg 1

# %%temp1_lshr_inner = powtab[shift]
function intrinsic:lshr/getshift

scoreboard players operation %param0%0 reg /= %%temp1_lshr_inner reg

# %%temp1_lshr_inner = powtab[31 - shift]
scoreboard players operation %param1%0 reg *= %%-1 reg
scoreboard players add %param1%0 reg 31
function intrinsic:lshr/getshift

execute if score %%temp0_lshr_inner reg matches 1..1 run scoreboard players operation %param0%0 reg += %%temp1_lshr_inner reg