# Arguments
# %ptr
# Return value is %return%0

function intrinsic:load_byte
scoreboard players operation %return%0 reg = %param0%0 reg
scoreboard players add %ptr reg 1

function intrinsic:load_byte
scoreboard players operation %param0%0 reg *= %%256 reg
scoreboard players operation %return%0 reg += %param0%0 reg
scoreboard players add %ptr reg 1

function intrinsic:load_byte
scoreboard players operation %param0%0 reg *= %%65536 reg
scoreboard players operation %return%0 reg += %param0%0 reg
scoreboard players add %ptr reg 1

function intrinsic:load_byte
scoreboard players operation %param0%0 reg *= %%16777216 reg
scoreboard players operation %return%0 reg += %param0%0 reg