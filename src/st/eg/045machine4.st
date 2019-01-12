App reset.
(Factory new: 'city') 
	newMachine: 'suburb';
	transition: (Transition new
			state: #waiting state;
			if:    [App random next > 0.8];
			then:  ['and , we are off' oo];
			next:  #running state);
	transition: (Transition new
			staysAt: #running state;
			then:  ['going to the pool' oo]);
	transition: (Transition new
			state: #running state;
			then: ['going home to sleep. Nite!' oo];
			next: #endF state);
	transition: (Transition new
			staysAt: #running state;
			then:  ['going to the shop' oo]);
	transition: (Transition new
			state:  #running state;
			then:  ['going to the store' oo];
			next:  #running state);
	transition: (Transition new
			state: #start state;
			then: ['i am staring ' oo];
			next: #waiting state);
	do: [:machine|
		machine inspect.
		machine transitions pairs: [:key :candidates| 
			key loo.
			candidates contents do: [:c| c oo ]]]
!
