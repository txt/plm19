local M={}

function M.same(x) return x end

function M.map(t, f, u)
  u = {}
  f = f or M.same
  for k,v in pairs(f) do u[k] = f(v) end
  return u
end

function M.ako(meta, x, inits)
  x = x or {}
  setmetatable(x,meta)
  for _,y in pairs(inits or {}) do x = x + y end 
  return x
end

return M
