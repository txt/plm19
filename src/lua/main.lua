local new=require("lib").new
local num=require("num")
local sym=require("sym")

a = new(num, {10,20,30,40,50}) 
b = new(num)
b = b + 20
print(a)
print(b)

c = new(sym, {"a","b","a","d","d","a"})
c = c + "d"
c = c + "d"
print(c, c:ent())
