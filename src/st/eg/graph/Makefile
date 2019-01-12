#### begin config section
#list the default test
X=0lib.st#

#where to find the test cases
Eg=eg#

# Banner for top of each test
Hi=printf "\n---| $X |-----------------------------------------\n\n"

#### end config. You should not need to adjust anything after this.

MAKEFLAGS+= -s -i 
Here	=  $(PWD)#
Tmp	=  $(HOME)/tmp/graph#
Testdir	=  .#
Tests 	:= $(shell cd $(Testdir); ls [0-9]*  | grep -v want  | sort -n)#
Run	=  gawk -f graph.awk #

all :  
	$(MAKE) X=$X test

ready:   $(Testdir) $(Tmp)

$(Tmp) :
	 - [ ! -d $(Tmp) ] && mkdir -p $(Tmp)

#### run all tests
tests:;  $(foreach x, $(Tests), $(MAKE) X=$x test;)

score :
	$(MAKE) tests | cut -d\  -f 1 | egrep '(PASSED|FAILED)' | sort | uniq -c 

#### run one test
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


