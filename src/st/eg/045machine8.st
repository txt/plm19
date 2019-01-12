|r|
App reset; asynchronousOff.
(Factory new: 'f1') 
	newMachine: 'trans1';
	go: #start to: #run then:   ['0' o];
	go: #run   to: #odd then:   ['1' o];
	go: #run   to: #even then:  ['2' o];
	go: #run   to: #endF if:    [App random next > 0.99];
	go: #odd   to: #run;
	go: #even  to: #run;
	run
!
