local ako=require("lib").ako
local sym={}

function sym.new(  inits) 
  return ako(sym, {n=0,most=0,most=0,seen={}}, inits) end

function sym.__tostring(i) 
   return tostring("Sym{".. i.n    ..", "
                         .. i.mode ..", "
			 .. i.most .."}") end

function sym.__add(i, x,      old)
  i.n       = i.n + 1
  i.seen[x] = (i.seen[x] or 0) + 1
  new       = i.seen[x]
  if new > i.most then
    i.mode, i.most = x, new end
  return i
end

return sym
