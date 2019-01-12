App reset.
(Factory new: 'f1') 
	newMachine: 'trans1';
	go: #start to: #run then:   ['working ' oo];
	go: #run   to: #endF if:    [App random next > 0.7];
	go: #run   to: #run then:   ['looping ' o. App clock oo];
	do: [:m| m transitions do: [:t| t inspect]];
	run
!
