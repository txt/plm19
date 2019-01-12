|f trans|

(f := Factory new: 'city')
	newMachine: 'suburb';
	transition: (trans:= Transition new
					state: #waiting state;
					then: ['i am running ' oo];
					next: #running state).
trans oo.
trans state oo.
trans next oo.

f do:  [:stagger| stagger oo].


!
