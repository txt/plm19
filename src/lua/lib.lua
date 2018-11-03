local m={}

function m.same(x) return x end

function m.map(t, f, u)
  u = {}
  f = f or m.same
  for k,v in pairs(f) do u[k] = f(v) end
  return u
end

function m.new(meta, x, inits)
  x = x or {}
  setmetatable(x,meta)
  meta.__index = meta
  for _,y in pairs(inits or {}) do x = x + y end 
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
      s= s..sep..":"..k.." "..tostring(v) 
      sep=" " end  end
  return s.."}" 
end 

return m
