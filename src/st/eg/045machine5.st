|f|
App reset.
(f:= Factory new: 'city')
	newMachine: 'person1';
	go: #waiting to: #running if:   [App random next > 0.8]
                              then: ['and, we are off' oo];
	go: #running to: #running then: ['going to the pool' oo];
	go: #running to: #running then: ['going to the shop' oo];
	go: #running to: #running then: ['going to the store' oo];
	go: #running to: #endF    then: ['going home to sleep. Nite!' o];
	go: #start   to: #waiting then: ['i am starting.' o];

 	newMachine: 'person2';
	go: #waiting to: #running if:   [App random next > 0.8]
                              then: ['and, we are off' oo];
	go: #running to: #running then: ['going to the pool' oo];
	go: #running to: #running then: ['going to the shop' oo];
	go: #running to: #running then: ['going to the store' oo];
	go: #running to: #endF    then: ['going home to sleep. Nite!' o];
	go: #start   to: #waiting then: ['i am starting.' o].
	
f machines contents do: [:machine|
		machine name loo.
		machine transitions pairs: [:key :candidates| 
			key loo.
			candidates contents do: [:c| c oo ]]]
!
