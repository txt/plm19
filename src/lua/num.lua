local ako=require("lib").ako
local num={}

function num.new(  inits) return ako(num, {sum=0,mu=0,n=0}, inits) end

function num.__tostring(n) 
   return tostring("Num{"..n.sum ..", ".. n.mu ..", ".. n.n .."}") end

function num.__add(n, x)
  n.sum = n.sum+x
  n.n   = n.n + 1
  n.mu  = n.sum/n.n
  return n
end

return num
