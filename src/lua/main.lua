local num=require("num")
local sym=require("sym")

a = num.new {10,20,30,40,50} 
b = num.new()
b = b + 20
print(a,b)

c = sym.new {"a","b","a","d","d","a"}
c = c + "d"
c = c + "d"
print(c, c:ent())
