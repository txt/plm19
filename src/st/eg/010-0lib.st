(#Fred isa:  Object 
       with: 'name age ') public !

|f|
f := Fred new name: 'flintsone'.
f name loo.
f name noo.
f name oo.
f name no.
f name o.
'' oo.
!

#(11 22 33) oo.
'to be or not to be' asWords oo. 
!

|i|
i := 0.
'eg/010-0lib.st' fileLinesDo: [:line| 
   i := i + 1.
        (i s, ':', Character tab s, line ) oo].
!

|x|
x ifNil: 	[x := 23].
x notNil: 	[x loo].
