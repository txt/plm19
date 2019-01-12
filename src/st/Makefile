#### begin config section
#list the default test
X=0lib.st#

#where to find the test cases
Eg=eg#

St=$(shell which gst)

# Banner for top of each test
Hi=printf "\n---| $X |-----------------------------------------\n\n"

#### end config. You should not need to adjust anything after this.

MAKEFLAGS+= -s -i 
Here	=	$(PWD)#
Tmp	=	$(HOME)/tmp#
Testdir	=	$(Here)/$(Eg)#
Tests 	:=	$(shell cd $(Testdir); ls *.st  | grep -v want  | sort -n)#
System	:=  	$(shell ls *.st) 
Run	 =	$(St)  $(System) #

all :  
	$(MAKE) X=$X test

ready:  $(St)  $(Testdir) $(Tmp)

$(Tmp) :
	 - [ ! -d $(Tmp) ] && mkdir $(Tmp)

#### run all tests
tests:;  $(foreach x, $(Tests), $(MAKE) X=$x test;)

score :
	$(MAKE) tests | cut -d\  -f 1 | egrep '(PASSED|FAILED)' | sort | uniq -c 

#### run one test
test : ready $(Testdir)/$(X).want
	$(MAKE) run > $(Tmp)/$X.got 
	if   diff -s $(Tmp)/$X.got $(Testdir)/$X.want > /dev/null;  \
        then echo PASSED $X ; \
        else echo FAILED $X:  expected $(Testdir)/$X.want, got $(Tmp)/$X.got; \
    fi

run : ready $(Testdir)/$(X) 
	$(Hi)
	$(Run) $(Testdir)/$(X)  

cache : ready
	$(MAKE) run | tee $(Testdir)/$X.want
	echo new test result cached to $(Testdir)/$X.want


