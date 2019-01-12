MAKEFLAGS+= -s -i 

#### standard stuff                                                                                                                                                                                              
Here=$(PWD)#
Tmp:=$(HOME)/tmp#
Testdir=$(Here)/eg#
Tests:=$(shell cd $(Testdir); ls *.st  | grep -v want  | sort -n)#
Group=11112222#
Proj=proj2a#
X=0lib.st#
310dir=cs310#
SubmitRoot=/home/timm/cs310/proj2#only works at csee                                                                                                                                                         
Submit=$(SubmitRoot)/$(USER)/$(310dir)/$(Proj)/$(Group)#

Run=bash st

main : all

ready:  st  $(Testdir) $(Tmp)

$(Tmp) :
	 - [ ! -d $(Tmp) ] && mkdir $(Tmp)


submit      : ; mkdir -p $(Submit); cp -vr * $(Submit) ; chmod o-r $(Submit)/*
submitLate1 : ; mkdir -p $(Submit)1; cp -vr * $(Submit)1 ; chmod o-r $(Submit)1/*
submitLate2 : ; mkdir -p $(Submit)2; cp -vr * $(Submit)2 ; chmod o-r $(Submit)2/*
submitLate3 : ; mkdir -p $(Submit)3; cp -vr * $(Submit)3 ; chmod o-r $(Submit)3/*
submitLate4 : ; mkdir -p $(Submit)4; cp -vr * $(Submit)4 ; chmod o-r $(Submit)4/*
submitLate5 : ;  mkdir -p $(Submit)5; cp -vr * $(Submit)5 ; chmod o-r $(Submit)5/*

#### run all tests
tests:;  $(foreach x, $(Tests), $(MAKE) X=$x test;)

score :
	$(MAKE) tests | cut -d\  -f 1 | egrep '(PASSED|FAILED)' | sort | uniq -c 

#### unit tests

test : ready $(Testdir)/$(X).want
	$(MAKE) run > $(Tmp)/$X.got 
	if   diff -s $(Tmp)/$X.got $(Testdir)/$X.want > /dev/null;  \
        then echo PASSED $X ; \
        else echo FAILED $X,  got $(Tmp)/$X.got; \
    fi

run : ready $(Testdir)/$(X) 
	$(Hi)
	$(Run) $(Testdir)/$(X)  

cache : ready
	$(MAKE) run | tee $(Testdir)/$X.want
	echo new test result cached to $(Testdir)/$X.want
