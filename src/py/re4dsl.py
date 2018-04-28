from pyparsing import Word, nums
parser = "LOGIN ALLOWED" + \
   Word(nums) + ":" + Word(nums) + \
   "TO" + \
   Word(nums) + ":" + Word(nums)
dsl = "LOGIN ALLOWED 09:00 TO 17:00"
print(parser.parseString(dsl))


import re
str ='LOGIN\s+ALLOWED\s+' +\
     '(\d{2}):(\d{2})' +\
     '\s+TO\s+' +\
     '(\d{2}):(\d{2})'
parser = re.compile(str)
dsl = "LOGIN ALLOWED 09:00 TO 17:00"
print(parser.match(dsl).groups())
