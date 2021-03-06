--[[
this file produces the actual module for lua-getch, combining the
C functionallity with the lua functionallity. You can use the C module
directly by requiring getch directly.
--]]

local ffi = require("ffi")

ffi.cdef[[
    int getch_blocking();
    void getch_non_blocking( int *outch);
]]

-- load C module
local getch = ffi.load("lua/libs/shared/getch.so")

-- return the combined module
return getch
