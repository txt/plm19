local m={}

function m.new(meta, inits,f,  x)
  x = meta.init and meta.init() or {}
  setmetatable(x,meta)
  meta.__index = meta
  meta.__tostring = function(z) 
    return m.show(z, meta.name) end
  f = f or function(z) return z end
  for _,y in pairs(inits or {}) do x = x + f(y) end 
  return x
end

function m.ordered(t,  i,keys)
  i,keys = 0,{}
  for key,_ in pairs(t) do keys[#keys+1] = key end
  table.sort(keys)
  return function ()
    if i < #keys then
      i=i+1; return keys[i], t[keys[i]] end end
end

function m.show(t,   pre,s,sep)
  pre   = pre or ""
  s,sep = pre.."{",""
  for k,v in m.ordered(t) do  
    if not k:match "^_" then
      if type(v) ~= "function" then
        s= s..sep..":"..k.." "..tostring(v) 
        sep=" " end  end end
  return s.."}" 
end 

return m
