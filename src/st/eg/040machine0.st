|f |

f := Factory new: 'my first factory'.
f 
	machine: (Machine new: 1 s);
	machine: (Machine new: 2 s);
	machine: (Machine new: 3 s);
	machine: (Machine new: 4 s);
	machine: (Machine new: 5 s).

f do: [:machine| machine oo.  ].
	
