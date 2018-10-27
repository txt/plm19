do
  local docs={}
  local img="https://raw.githubusercontent.com/researchart/re19/master/img/"
  local img="https://git.io/xxx15"
  function def(t,   name,fun,doc)
    for k,v in pairs(t) do
      if type(v) == "string"
        then name, doc = k,v
        else fun = v end end
    _G[name] = fun
    doc = doc:gsub("^[ \t]+","")
             :gsub("\n[^:][ \t]+","\n")
             :gsub("\n:","\n")
             :gsub("_IMG%(","![]("..img)
    print(doc)
    docs[name] = doc
  end
end

def { love = [[# asdasddas
    
    Never seen asdas
    asdasas asas
       	      
    - fred
    - fereasda
	      
    ## sdas asda

    _HOME[asdsa]
    _IMG(adadsasda)
      
    _sdas_  asdad
	      
    :    function adsa(x) return x end

    ]],
    function (x) return x+2 end }



print(love(20))

