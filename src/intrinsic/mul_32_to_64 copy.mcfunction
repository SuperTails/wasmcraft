# Arguments:
# %param0%0 - Left operand
# %param1%0 - Right operand
# %return%0 - Low word of product
# %return%1 - High word of product

scoreboard players operation %%tempmul_p0_save reg = %param0%0 reg
scoreboard players operation %%tempmul_p1_save reg = %param1%0 reg

scoreboard players operation %param0%0 reg = %%tempmul_p0_save reg
scoreboard players operation %param1%0 reg = %%tempmul_p1_save reg