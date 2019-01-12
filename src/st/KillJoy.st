! KillJoy class methodsFor: 'creation' !
new: name
	^(super new: name) init
!
worksIn: factory
    factory 
       machine: (self new: 'party boy');
       go: #start   to: #run     then:  [The machine talks: 0];
       go: #run     to: #oddHaHa then:  [The machine talks: 1];
       go: #run     to: #even    then:  [The machine talks: 2];
       go: #oddHaHa to: #oddHaHa then:  [The machine 
                                            drinks; 
                                            talks: 3];
       go: #oddHaHa to: #run     if:    [The machine happiness < 2 ] 
                                 then:  [The machine 
                                            talks: 4;
                                            talks: 'Too drunk to stay awake.'];
       go: #even    to: #run;
       go: #run     to: #endF    if:    [App random next > 0.9] 
                                 then:  [The machine 
                                            talks: 5;
                                            talks: 'that is all folks'.
                                         The yourself oo.
                                         The state oo.
                                         The next oo.
                                         The machine oo.
                                         The factory oo].
!!

"============================================"
! KillJoy methodsFor: 'creation' !
init
    happiness := 10
!!

"============================================"
! KillJoy methodsFor: 'operations' !
drinks
    happiness := happiness sqrt
!
happiness
    ^happiness
!!

"============================================"
! KillJoy methodsFor: 'printing' !
talks: x
    ('Machine:[', id s, '] ', x s)o
!!
