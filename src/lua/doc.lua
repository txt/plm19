DOCS={}

function def(t,   name,fun,doc)
  for k,v in pairs(t) do
    if type(v) == "string"
      then name, doc = k,v
      else fun = v end end
   _G[name] = fun
   DOCS[name] = doc:gsub("^[ \t]+","")
                   :gsub("\n[^:][ \t]+","\n")
	           :gsub("\n:","\n")
end

def { love = [[# asdasddas
    
    Never seen asdas
    asdasas asas
       	      
    - fred
    - fereasda
	      
    ## sdas asda
      
    _sdas_  asdad
	      
    :    function adsa(x) return x end

    ]],
    function (x) return x+2 end }


for k,v in pairs(DOCS) do
  print(v) end

print(love(20))

