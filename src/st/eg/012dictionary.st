|d old new|
d := Dictionary new.
'i have gone to seek a great perhaps' do: [:sym|
	old := d at: sym ifAbsent: [0].
	d at: sym put: old+1
].
d pairs: [:key :value| ('[',key s, ']=[',value s,'] ') oo].  
!
