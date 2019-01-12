App reset; asynchronousOff.
(Factory new: 'f1') 
    newMachine: 'trans1';
    go: #start    to: #run      then:  [0 o];
    go: #run      to: #oddHaHa  then:  [1 o];
    go: #run      to: #even     then:  [2 o];
    go: #oddHaHa  to: #oddHaHa  then:  [3 o];
    go: #oddHaHa  to: #run      if:    [App random next > 0.7] then: [4 o];
    go: #even     to: #run;
    go: #run      to: #endF     if:    [App random next > 0.9] 
                                then:  [5 noo.
                                    'that is all folks' oo.
                                    The yourself oo.
                                    The state oo.
                                    The next oo.
                                    The machine oo.
                                    The factory oo];
    run
!
