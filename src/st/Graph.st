"design note: this is a MODEL class that should not be polluted with
 DATA concepts (e.g. reading graph spec)
"
! Graph class methods !
new
	^super new init
!!
! Graph methods !
frame
       ox to: (width  - 1) do: [:i| self plot: i  @ oy           with: $- ].
       ox to: (width  - 1) do: [:i| self plot: i  @ (height - 1) with: $- ].
       oy to: (height - 1) do: [:i| self plot: ox @ i            with: $| ].
       oy to: (height - 1) do: [:i| self plot: (width - 1) @ i   with: $| ] 
!
height
	^height
!
height: h 
	self height: h width: width 
!
height: h width: w
	height := h.
	width  := w.
	self resize 
!
init
	label  := ''. 
	xrange := Range new.
	yrange := Range new.
	matrix := OrderedCollection new.
	self height: 0  width: 0.
	ox=6.
	oy=2.
!
resize 
       matrix := Array new: height.
       1 to: height do: [:i|
           matrix at: i put: (Array new: width) ].
!
plot: aPoint with: aSymbol	
      (matrix at: aPoint y) at: aPoint x put: aSymbol. 
!
splot: aPoint with: aString
       |i|
       i := 0.
       aString do: [:elm |
       	    self plot: (aPoint x + i) @ aPoint y with: elm.
            i := i + 1 ]
!
width
	^width
!
width: aNumber
	^width := aNumber
!
wxidth: w 
       self height: height width: w
!
xscale: aNumber
	^((xrange scale: aNumber) * (width - 1 - ox) + ox) round 
!
yscale: aNumber
	^((yrange scale: aNumber) * (height - 1 - oy) + oy) round 
!!
