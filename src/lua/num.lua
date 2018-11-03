local lib = require("lib")
local num = {}

function num.new(  inits) 
  return lib.new(num, {sum=0,mu=0,n=0}, inits) 
end

function num:__tostring() 
  return lib.show(self,"Num") 
end

function num:__add(x)
  self.sum = self.sum+x
  self.n   = self.n + 1
  self.mu  = self.sum/self.n
  return self
end

return num
