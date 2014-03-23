--------------------------------------------------------------------------------
-- A general purpose library that extends Lua standard libraries.
-- 
-- Copyright (C) 2011-2014 Stefano Peluchetti. All rights reserved.
--
-- Features, documentation and more: http://www.scilua.org .
-- 
-- This file is part of the Xsys library, which is released under the MIT 
-- license: full text in file LICENSE.TXT in the library's root folder.
--------------------------------------------------------------------------------

-- TODO: Design exec API logging so that files are generated (useful for 
-- TODO: debugging and profiling).

local ffi     = require "ffi"
local bit     = require "bit"
local cfg     = require "xsys.cfg"
local templet = require "xsys._dep.templet"

-- User-definable options:
local logexec    = cfg.log.exec

local select, pairs, error, setmetatable = select, pairs, error, setmetatable
local type, loadstring, setfenv, unpack = type, loadstring, setfenv, unpack
local pcall = pcall
local insert, concat = table.insert, table.concat
local format = string.format
local abs = math.abs

-- Table -----------------------------------------------------------------------
local function merge(...)
  local o = {}
  local arg, n = { ... }, select("#", ...)
  for a=1,n do
    for k,v in pairs(arg[a]) do
      if type(o[k]) ~= "nil" then
        error("key '"..tostring(k).."' is not unique among tables to be merged")
      end
      o[k] = v
    end
  end
  return o
end

local function join(...)
  local o = { }
  local arg, n = { ... }, select("#", ...)
  local c = 0
  for a=1,n do
    local t = arg[a]
    for i=1,#t do
      c = c + 1
      local v = t[i]
      if type(v) == "nil" then
        error("argument #"..a.." is not a proper array: no nil values allowed")
      end
      o[c] = v
    end
  end
  return o
end

local table = merge(table, {
  merge = merge,
  join  = join,
})

-- String ----------------------------------------------------------------------
-- CREDIT: Steve Dovan snippet.
local function split(s, re)
  local i1, ls = 1, { }
  if not re then re = '%s+' end
  if re == '' then return { s } end
  while true do
    local i2, i3 = s:find(re, i1)
    if not i2 then
      local last = s:sub(i1)
      if last ~= '' then insert(ls, last) end
      if #ls == 1 and ls[1] == '' then
        return  { }
      else
        return ls
      end
    end
    insert(ls, s:sub(i1, i2 - 1))
    i1 = i3 + 1
  end
end

-- TODO: what = "lr"
local function trim(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

local function width(x, chars)
  chars = chars or 9
  if chars < 9 then
    error("at least 9 characters required")
  end
  if type(x) == "nil" then
    return (" "):rep(chars - 3).."nil"
  elseif type(x) == "boolean" then
    local s = tostring(x)
    return (" "):rep(chars - #s)..s
  elseif type(x) == "string" then
    if #x > chars then
      return x:sub(1, chars - 2)..".."
    else
      return (" "):rep(chars - #x)..x
    end
  else
    local formatf = "%+"..chars.."."..(chars - 3).."f"
    local formate = "%+."..(chars - 8).."e"
    x = tonumber(x) -- Could be cdata.
    local s = format(formatf, x)
    if x ~= x or abs(x) == 1/0 then return s end
    if tonumber(s:sub(2, chars)) == 0 then -- It's small.
      if abs(x) ~= 0 then -- And not zero.
        s = format(formate, x)
      end
    else
      s = s:sub(1, chars)
      if not s:sub(3, chars - 1):find('%.') then -- It's big.
        s = format(formate, x)
      end
    end
    return s
  end
end

local string = merge(string, {
  split = split,
  trim  = trim,
  width = width,
})

-- Template --------------------------------------------------------------------
-- CREDIT: Peter Colberg's Templet library.
local template = templet.loadstring

-- Exec ------------------------------------------------------------------------
local function testexec(chunk, chunkname, fenv, ok, ...)
  if not ok then
    local err = select(1, ...)
    error("execution error: [[ "..err.." ]] in chunk: [[\n"..chunk.."]]")
  end
  if logexec then -- Add location?
    logexec(chunk, chunkname, fenv)
  end
  return ...
end

local function exec(chunk, chunkname, fenv)
  chunkname = chunkname or chunk
  local f, e = loadstring(chunk, chunkname)
  if not f then
    error("parsing error: [[ "..e.." ]] in chunk: [[\n"..chunk.."]]")
  end
  if fenv then 
    setfenv(f, fenv)
  end
  return testexec(chunk, chunkname, fenv, pcall(f))
end

-- From ------------------------------------------------------------------------
local function from(what, keystr)
  local keys = split(keystr, ",")
  local o = { }
  for i=1,#keys do
    o[i] = "x."..trim(keys[i])
  end
  o = concat(o, ",")
  local s = "return function(x) return "..o.." end"
  return exec(s, "from<"..keystr..">")(what)
end

-- Copy ------------------------------------------------------------------------
local copy_type = { } -- Dispatch table for copying single object based on type.

local function copy_single(x)
	return copy_type[type(x)](x)
end

-- Table objects with optional copy member function.
-- Metatables are always shared.
copy_type.table = function(x)
  if x.copy then 
    return x:copy()
  else
    local o = { }
    for k, v in pairs(x) do
      o[k] = copy_single(v)
    end
    return setmetatable(o, getmetatable(x))
  end
end

-- Cdata objects requires copy member function.
copy_type.cdata = function(x)	return x:copy() end

-- Closures are shared.
copy_type["function"] = function(x) return x end

-- These have value semantics no copy required.
copy_type.string  = function(x) return x end
copy_type.number  = function(x) return x end
copy_type.boolean = function(x) return x end
copy_type["nil"]	= function(x) return x end

-- Perform a "by-value" copy of multiple tables or cdata objects.
local function copy(...)
	local o = { }
	for i=1,select("#",...) do
		o[i] = copy_single(select(i, ...))
	end
	return unpack(o)
end

-- Bit -------------------------------------------------------------------------
local tobit, lshift, rshift, band = bit.tobit, bit.lshift, bit.rshift, bit.band 

-- 99 == not used.
local lsb_array = ffi.new("const int32_t[64]", {32, 0, 1, 12, 2, 6, 99, 13,
  3, 99, 7, 99, 99, 99, 99, 14, 10, 4, 99, 99, 8, 99, 99, 25, 99, 99, 99, 99,  
  99, 21, 27, 15, 31, 11, 5, 99, 99, 99, 99, 99, 9, 99, 99, 24, 99, 99, 20, 26, 
  30, 99, 99, 99, 99, 23, 99, 19, 29, 99, 22, 18, 28, 17, 16, 99})

-- Compute position of least significant bit, starting with 0 for the tail of
-- the bit representation and ending with 31 for the head of the bit
-- representation (right to left). If all bits are 0 then 32 is returned.
-- This corresponds to finding the i in 2^i if the 4-byte value is set to 2^i.
-- Branch free version.
local function lsb(x)
  x = band(x, -x)
  x = tobit(lshift(x, 4) + x)
  x = tobit(lshift(x, 6) + x)
  x = tobit(lshift(x, 16) - x)
  return lsb_array[rshift(x, 26)]
end

local bit = merge(bit, {
  lsb = lsb,
})

-- Export ----------------------------------------------------------------------

return {
  template = template,
  exec     = exec,
  from     = from,
  copy     = copy,

  table    = table,
  string   = string,
  bit      = bit,
}