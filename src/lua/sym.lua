local lib = require("lib")
local sym = {}

function sym.new(  inits) 
  return lib.new(sym, {n=0,most=0,most=0,_seen={}}, inits) 
end

function sym:__tostring() 
   return lib.show(self,"Sym")
end

function sym:__add(x,      new)
  self.n        = self.n + 1
  self._seen[x] = (self._seen[x] or 0) + 1
  new           = self._seen[x]
  if new > self.most then
    self.mode, self.most = x, new end
  return self
end

function sym:ent(   e,p)
  e = 0
  for _,v in pairs(self._seen) do
    p = v/self.n
    e = e - p*math.log(p,2)
  end
  return e
end

return sym
