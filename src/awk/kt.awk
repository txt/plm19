# syntax: GAWK -f KNIGHTS_TOUR.AWK [-v sr=x] [-v sc=x]
#
# examples:
#   gawk -f kt.awk [n] [row1] [col1]
#
# defaults to n=8 and knight starts at row1=1 and col1=1
BEGIN {
    split("2  2 -2 -2 1  1 -1 -1",X," ")
    split("1 -1  1 -1 2 -2  2 -2",Y," ")
    N=ARGV[1]
    N=N?N:8
    main(ARGV[2],ARGV[3])
}
function main(sr,sc) {
    sr = sr?sr: 1
    sc = sc?sc: 1
    printf("\n#%dx%d board: starting row=%d col=%d\n",N,N,sr,sc)
    move(sr,sc,0)
    exit(1)
}
function move(x,y,m) {
    if (cantMove(x,y))  return(0) 
    P[x,y] = ++m
    if (m == N ^ 2) 
      printBoard()
    else
      tryBestMove(x,y,m)
}
function cantMove(x,y) {
    return( P[x,y] || x<1 || x>N || y<1 || y>N )
}
function tryBestMove(x,y,m,  i) {
    i = bestMove(x,y)
    move(x+X[i],y+Y[i],m)
}
function bestMove(x,y,  arg1,arg2,c,i,min,out) {
# Warnsdorff's rule: go to where there are fewest next moves
    min = N ^ 2 + 1
    for (i in X) {
      arg1 = x + X[i]
      arg2 = y + Y[i]
      if (!cantMove(arg1,arg2)) {
        c = countNext(arg1,arg2)
        if (c < min) {
          min = c
          out = i 
    }}}
    return(out)
}
function countNext(x,y,  i,out) {
    for (i in X) 
      out += (!cantMove(x+X[i],y+Y[i]))
    return(out)
}
function printBoard(  i,j,leng) {
    leng = length(N*N)
    for (i=1; i<=N; i++) {
      for (j=1; j<=N; j++) 
        printf(" %*d",leng,P[i,j])
      printf("\n")
}}
