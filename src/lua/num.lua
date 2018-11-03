local num = {name="Num"}

function num.init() 
  return {mu=0,m2=0,sd=0,n=0,
          lo= math.maxinteger,
	  hi= math.mininteger}
end

function num:__add(x,     d)
  if x == "?" then return self end
  self.n = self.n + 1
  d = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  if x > self.hi then self.hi = x end
  if x < self.lo then self.lo = x end
  if (self.n>=2) then 
     self.sd = (self.m2/(self.n - 1 + 10^-32))^0.5 end
  return self
end

function num:__sub(x,    d)
  if (x == "?") then return self end
  if (self.n == 1) then return self end
  self.n  = self.n - 1
  d = x - self.mu
  self.mu = self.mu - d/self.n
  self.m2 = self.m2 - d*(x- self.mu)
  if (self.n>=2) then 
     self.sd = (self.m2/(self.n - 1 + 10^-32))^0.5 end
  return self
end

return num
