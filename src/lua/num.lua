local num = {name="Num"}

function num.init() 
  return {mu=0,m2=0,sd=0,n=0,
          lo= 10^32,
	  hi= -1*10^32}
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

function num:same(other, conf,small)
  return self:insignificant(other, conf ) and 
         self:small(        other, small)
end

function num:insignificant(other,  conf, df,xs,ys)
  conf = conf or 0.95
  df   = math.min(i.n - 1, j.n - 1)
  xs   = {     1,    2,    5,   10,   20,  30,   60, 100}
  ys   = {}
  ys[0.9] = { 3.078,1.886,1.476,1.372,1.325,1.31, 1.296,1.29}
  ys[0.95]= { 6.314,2.92, 2.015,1.812,1.725,1.697,1.671,1.66}
  ys[0.99]= {31.821,6.965,3.365,2.764,2.528,2.457,2.39,2.364}
  return (abs(self.mu - other.mu) / 
          ((self.sd()/self.n + other.sd()/other.n)^0.5) 
	 ) < interpolate(df, xs, ys[conf])
end

function num:small(other,    small)
  small = small or 0.38
  num   = (self.n - 1)*self.sd^2 + (other.n - 1)*other.sd^2
  denom = (self.n - 1) + (other.n - 1)
  sp    = ( num / denom )^0.5
  delta = abs(self.mu - other.mu) / sp
  c     = 1 - 3.0 / (4*(self.n + other.n - 2) - 1)
  return delta * c < small
end

local function abs(x) return x > 0 and x or -1*x end

local function interpolate(x,xs,ys,          x0,y0,x1,y1)
  x0, y0 = xs[1], ys[1]
  if x <= x0 then return y0  end
  if x > xs[ #xs ] then return ys[ #ys ] end
  for i = 1, #xs do
    x1,y1 = xs[i],ys[i]
    if x0 <= x and x < x1 then break end
    x0, y0 = x1, y1 end
  return y0 +  (x - x0)/(x1 - x0) * (y1 - y0)
end

return num
