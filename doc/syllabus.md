[home](http://tiny.cc/plm19) |
[copyright](https://github.com/txt/plm19/blob/master/license.md) &copy;2019, timm&commat;ieee.org
<br>
[<img width=900 src="https://raw.githubusercontent.com/txt/plm19/master/etc/img/banner.png">](http://tiny.cc/plm19)<br>
[syllabus](https://github.com/txt/plm19/blob/master/doc/syllabus.md) |
[src](https://github.com/txt/plm19/tree/master/src) |
[submit](http://tiny.cc/plm19give) |
[chat](https://plm19.slack.com/)


# Syllabus

Theory of Programming Languages   
NCSU, CSC       
Spring 2019  
3 units  
CSC 417-991 (11904)  
Tues/Thurs 4:30 to 5:45     
EE I, Room 	1007    



In your professional life you will work with dozens of different programming languages.
What can you learn from 100s of past languages to help you work with your future languages?



## Synopsis


Theory of programming languages with emphasis on programming
language abstractions and implementation issues. Formal models of
syntax and semantics. Static versus dynamic scoping. Parameter passing
mechanisms. Garbage collection. Programming in alternate paradigms such as
applicator, functional, logic, and object-oriented programming languages.


## Staff

### Lecturer

Tim Menzies (Prof)


+ Office Hours: Tuesday, 2:00-4:00 and by request
+ Location of Office Hours: EE II room 3298
+ Slack name: timm
+ Github name: timm
+ E-Mail: timm@ieee.org
  + Only use this email for private matters. All other class 
    communication should be via the class Slack group [http://plm19.slack.com](http://plm19.slack.com).
+ Phone: 304-376-2859
       + **Do not use** this number, except in the most dire of
          circumstances (best way to contact me is via email).


### Teaching assistant

Rahul Krishna, (CSC, Ph.D., May 2019).



+ Office Hours: Monday 11 to 1pm
+ Location of Office Hours: EE2, rm3240
+ Slack name: r.krsn
+ Github name: rahlk


## Group mailing list

During term time, all communication will be via
the Slack group https://plm19.slack.com.
. Students are strongly encouraged to contribute their questions and answers to that shared resource.
+ Note that, for communication of a more private nature, contact the lecturer on the email shown above.


## Prerequisite 

Prerequisite: CSC 316 or ECE 309 or equivalent.

## Texts

### Required


[Exercises in Programming Style](https://www.amazon.com/Exercises-Programming-Style-Cristina-Videira/dp/1482227371)     
by Cristina Videira Lopes    
Chapman and Hall/CRC; 1 edition (June 4, 2014)    
ISBN-10: 1482227371

- [code available on-line](https://github.com/crista/exercises-in-programming-style)
- [Introductory slides](http://gotocon.com/dl/goto-aar-2013/slides/CristinaVideiraLopes_ExercisesInStyle.pdf)


### Suggested

Domain-Specific Languages   
by Martin Fowler   
Addison-Wesley Professional; 1 edition (October 3, 2010)   
ISBN-10: 0321712943 

- [Catalog of DSL patterns](https://martinfowler.com/dslCatalog/)


## Expected Workload
Note that this is a
**programming-intensive** subject that covers many languages
_only some of which the lecturer/GTA have used_.
You will be spending much time in the process of
self-education of different languages.


Sometimes, the lecturer/tutor will require you to attend a review session during their consultation  time. There, students may be asked to review
code, concepts, or comment on the structure of the course. Those sessions are mandatory and failure to attend will result in marks being deducted.

Students MUST be
prepared to dedicate AT LEAST 5-8 working hours a
week to this class (excluding the time spent in the
classroom). Laboratory instruction is not included
in this subject.

## Grades

The following grade scale will be used:

+ A+  (97-100), A (93-96), A-(90-92)
+ B+ (87-89), B (83-86), B-(80-82)
+ C+ (77-79), C (73-76), C-(70-72)
+ D+ (67-69), D (63-66), D-(60-62)
+ F (below 60).

Grades will be added together using:

+ Mid-term  : 24  marks
+ Final Exam : 30 marks
+ Project: 45 marks
    - Work in groups of 3 (Groups  created randomly, see http://tiny.cc/plm19give)

Due dates for the above will shown on subject home page. Late submissions will earn 1 late mark per day (weekend is 1).

## Project


Part1 : 16 marks

- 1+ Grading some other group's homeworks 1abcde (1 mark each). Report to tutor if code does not run
  or if the I/O behavior is not as it should be.
- 1: Writing 4 homeworks 1abcde  (3 mark each)

Part2 : 30 marks (10 per part)

- 2a : Given a data mining pipeline (from the lecturer), replace any one parts of the pipe using
	  a different programming language. 
     - FIRST, write a reporting ranking ten abstractions you are thinking
	  of trying for that code (where  list includes a short description of each AND a tiny example where that 
	  abstraction might be useful).  For a list of abstractions, see below.
     - SECOND, write working code that replicates the i/o of that part of pipe.
	  That code should include your attempt to use your top three ranked abstractions (and it is expected
	  that you some of your planned abstractions will prove useless). 
     - THIRD add to the report an epilogue
	  describing your experience with the abstractions AND your recommendations to other people about
	  when to use/to avoid those abstractions. 
     - FOURTH add an end section describing what maximum grades you expect for this section (see below _bonus marks_).
     - FIFTH in some public Github repo (not from NCSU) write
	  a sub-directory called "_2a_". Add to that directory your report in pdf format  (I expect 5 pages (no less or more),
	  [2 column conference format](https://www.overleaf.com/gallery/tagged/acm-official#.WOuOk2e1taQ) AND
	  your working code AND a file canned "run" (that graders will run) AND a text file "run.out" showing the input and output when you run the code.
- 2b : as per 2a but use a different language, and a different part of the pipe and store the outputs to
         a directory "_2b".
- 2c : Grading someone else's group 2 code (not the written report, just checking it works as advertised).
- 2d : bonus marks. as per 2c. Only if allowed by professor. And
   this bonus mark is due same time as 2ab.

List of abstractions

1. See class
2. See the design patters list
3. From the Lopez book
4. From the Fowler book.

Students can develop their project anywhere, anyhow they like but
when they deliver:

- Code must be in a github repo (one per group), not NCSU
- Code must run on command-line Ubunty 16+ on 
  [codeanywhere.com](https://codeanywhere.com/pricing)
- Code must auto-install; i.e. the run script should check the
  environment for missing parts then install those parts.

For example:

```
for i in vim lua clisp  
do if   which $i
   then echo "# $i installed" 
   else sudo apt-get install $i 
   fi
done
```
There is a free [codeanywhere.com](https://codeanywhere.com/pricing)
plan. But, optionally,
students may want to consider _Freelancer_ package.


## Bonus Marks

- Some languages are harder than others. E.g. Python? Easy! But Haskell? Harder! 
- Some parts of the pipe are harder than others.
- Some abstractions are harder than others. E.g. polymorphism? Easy! But monads? Harder!

Later in the term we will we issue _stars_ for each language/parts/abstractions. 

- Zero bonus marks for using _one star_ things. 
- 7% bonus for using _two stars_ things.
- 14% for using _three stars_ things. 

The marks are multiplicative.
So if you use one star things through out then you can score 20 marks for parts 2b,2c.
But if you three star languages, parts, abstractions then 
that would be 20\*1.14<sup>3</sup> = 30 marks (actually, 29.6 which we'd round to 30).


## Attendance

Attendance is extremely important for your learning
experience in this class. Once you reach three
unexcused absences, each additional absence will
reduce your attendance grade by 10%.

Except for officially allowed reasons, your presence in the class if required from day one.
Late-comers will have to work in their own solo groups (to avoid disruptions to existing groups).

Note that absences for weddings (your own, or someone else's, is not an officially allowed reason).

Exceptions: this subject  will support students who are absent for any of the following
officially allowed reasons:

- Anticipated Absences (cleared with the instructor before the absence).
Examples of anticipated situations include
    - representing an official university function, e.g., participating in a professional meeting, as part of a judging team, or athletic team;
    - required court attendance as certified by the Clerk of Court;
    - religious observances as verified by the Division of Academic and Student Affairs (DASA).
    - Required military duty as certified by the student's commanding officer.
- Unanticipated Absences.  Excuses must be reported to the instructor not more than one week after the return to class.  Examples of unanticipated absences are:
      -  Short-term illness or injury affecting the ability to attend or to be productive academically while in class, or that could jeopardize the health of the individual or the health of the classmates attending.  Students must notify instructors prior to the class absence, if possible, that they are temporarily unable to attend class or complete assignments on time.
      -  Death or serious illnesses in the family when documented appropriately.  An attempt to verify deaths or serious illness will be made by the Division of Academic and Student Affairs.

That support will include changing the schedule of deliverables and/or (in extreme
case) different grading arrangements.


## Academic Integrity

Cheating will be punished to the full extent permitted. Cheating
includes plagerism of other people's work. All students will be working
on public code repositories and **informed reuse** is encouraged where
someone else's product is:

+ Imported and clearly acknowledged (as to where it came from);
+ The imported project is understood, and
+ The imported project is significantly extended.

Students are encouraged to read each others code and report **uninformed reuse**
to the lecturer. The issue will be explored and, if uncovered,
cheating will be reported to the university
and marks will be deducted if the person who is doing the reuse:

+ Does not acknowledge the source of the product;
+ Does not exhibit comprehension of the product when asked about it;
+ Does not significantly extend the product.

All students are expected to maintain traditional
standards of academic integrity by giving proper
credit for all work.  All suspected cases of
academic dishonesty will be aggressively pursued.
You should be aware of the University policy on
academic integrity found in the Code of Student
Conduct.

The  exams will be done individually.  Academic integrity is important.  Do not work together on the exams: cheating on either will be punished to the full extent permitted.

## Disabilities

Reasonable accommodations will be made for students
with verifiable disabilities. In order to take
advantage of available accommodations, students must
register with Disability Services for Students at
1900 Student Health Center, Campus Box 7509,
919-515-7653. For more information on NC State's
policy on working with students with disabilities,
please see the Academic Accommodations for Students
with Disabilities Regulation(REG 02.20.01).

Students are responsible for reviewing the PRRs
which pertain to their course rights and
responsibilities. These include:
http://policies.ncsu.edu/policy/pol-04-25-05 (Equal
Opportunity and Non-Discrimination Policy
Statement), http://oied.ncsu.edu/oied/policies.php
(Office for Institutional Equity and
Diversity),http://policies.ncsu.edu/policy/pol-11-35-01
(Code of Student Conduct), and
http://policies.ncsu.edu/regulation/reg-02-50-03
(Grades and Grade Point Average).

## Non-Discrimination Policy

NC State University provides equality of opportunity
in education and employment for all students and
employees. Accordingly, NC State affirms its
commitment to maintain a work environment for all
employees and an academic environment for all
students that is free from all forms of
discrimination. Discrimination based on race, color,
religion, creed, sex, national origin, age,
disability, veteran status, or sexual orientation is
a violation of state and federal law and/or NC State
University policy and will not be
tolerated. Harassment of any person (either in the
form of quid pro quo or creation of a hostile
environment) based on race, color, religion, creed,
sex, national origin, age, disability, veteran
status, or sexual orientation also is a violation of
state and federal law and/or NC State University
policy and will not be tolerated.

+ Note that, as a lecturer, I am legally required to
  **report** all such acts to the campus policy.

Retaliation
against any person who complains about
discrimination is also prohibited. NC State's
policies and regulations covering discrimination,
harassment, and retaliation may be accessed at
http://policies.ncsu.edu/policy/pol-04-25-05 or
http://www.ncsu.edu/equal_op/. Any person who feels
that he or she has been the subject of prohibited
discrimination, harassment, or retaliation should
contact the Office for Equal Opportunity (OEO) at
919-515-3148.

## Other Information

Non-scheduled class time for field trips or
out-of-class activities are NOT required for this
class. No such trips are currently planned. However,
if they do happen then students are required to
purchase liability insurance. For more information,
see http://www2.acs.ncsu.edu/insurance/




