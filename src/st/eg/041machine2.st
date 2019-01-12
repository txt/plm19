|f machine trans|

machine := Machine new: 'new'.
waiting := #waiting state.
running := #running state.


machine
	add: (Transition new
			state: 	waiting;
			if:    	[App random next > 0.8];
			then:  	['and , we are off' oo];
			next:  	running);
	
	add: (Transition new
			state: 	running;
			then:  	['going to the pool' oo];
			next: 	running);

	add: (Transition new
			state: 	running;
			then: 	['going home to sleep. Nite!' oo];
			next: 	waiting
		);

	add: (Transition new
			state: 	waiting;
			then: 	['going home to sleep. Nite!' oo];
			next: 	#loveShack state
		).
	

machine inspect.
machine transitions pairs: [:key :candidates| 
	key noo.
	candidates do: [:c| c oo]
]
!
