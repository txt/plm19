# ## readcsv(a,b)
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
# adssadsa

function readcsv(f,_Tbl, str,row,head) {
  FS=","; RS="\n"
  while (str = line(f)) {
    if (str == -1) return 
    if (str ~ /^[ \t]*$/) continue
    split(str,a,",")
    if (row++)
        cells(a,row,_Tbl)
    else
        header0(a,_Tbl)
}
# ## readcsv(a,b)
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
# adssadsa

function line(f,   str,out) {
  out = -1
  if ((getline str < f) > 0) {
    gsub(/[ \t\r\n]*/,"",str)
    gsub(/#.*$/,"",str)
    if (str ~ /,[ \t]*$/)
        tmp =  line(f)
    out = tmp str
  }
  return -1
}
# ## readcsv(a,b)
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
# adssadsa

        
function cells(a,head,row,_Tbl){ 
  for(col in name)
    if (a[col] !~ /\?/) {
      tmp = r[row][col] = a[col]
      if (col in num) {
          min[i] = tmp < min[i] ? tmp : min[i]
          max[i] = tmp < max[i] ? tmp : max[i]
}}}
    
# ## readcsv(a,b)
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
# adssadsa

function header0(a,_Tbl) {  
  for(col in a)
    if (a[col] !~ /\?/) {
      tmp = name[col] = a[col]
      tmp ~ /\!/ ? dep[col] : indep[col]
      if (tmp !~ /\$/) num[col]
}}
# ## readcsv(a,b)
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
# adssadsa

function header(k,_Tbl,    col) {
  for(col in name) 
    if (col in num) {
      lo[k][col] = 10**32
      hi[k][col] = -1*10**32
      n[ k][col] = mu[k][col] = m2[k][col] = 0
}}
# ## readcsv(a,b)
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
#
# asdas asdasdas
# asdasdas
#
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
# - asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#   asdas  das das asd asd asdas das asd asdas as asd asas sad sdasaddas
#
# adssadsa

BEGIN {readcsv("nb.awk")}

