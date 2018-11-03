local sqrt = math.sqrt
local _ENV = {}

function new(x,y) return {x=x,y=y} end
function add(u,v) return {x=u.x+v.x, y=u.y+v.y} end
function norm(u)  return sqrt(u.x^2 + u.y^2) end 

return _ENV

