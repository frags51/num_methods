real function getRootWithBisection(xLow, xHigh, tolerance)
implicit none
real, external :: getFunction
logical, external :: haveOppositeSigns
real, intent(inout) :: xLow, xHigh
real, intent(in) :: tolerance
real :: error
integer :: iteration
real :: fLow, fHigh
real :: xMid, fMid
write(*,*)
write(*,*) &quot;Bisection Method&quot;
write(*,*)
if(xLow &gt; xHigh) then
write(*,*) &quot;The lower bound is greater than the upper bound. Swapping...&quot;
write(*,*)
call swapNumbers(xLow, xHigh)
end if
fLow = getFunction(xLow)
fHigh = getFunction(xHigh)
if(fLow == 0) then
write(*,*) &quot;The initial guess for xLow is the exact root.&quot;
write(*,*)
getRootWithBisection = xLow
return
end if
if(fHigh == 0) then
write(*,*) &quot;The initial guess for xHigh is the exact root.&quot;
write(*,*)
getRootWithBisection = xHigh
return
end if
if(haveOppositeSigns(fLow, fHigh) .eqv. .false.) then
stop &quot;Bisection Method: Bounds don&#39;t bracket the root.&quot;
end if
error = abs((xHigh - xLow) / 2)
iteration = 0
xMid = (xLow + xHigh) / 2
fMid = getFunction(xMid)
write(*,10) &quot; | &quot;, &quot;No.&quot;, &quot; | &quot;, &quot;xLow&quot;, &quot; | &quot;, &quot;xHigh&quot;, &quot; | &quot;, &quot;fLow&quot;, &quot; | &quot;,
&quot;xMid&quot;, &quot; | &quot;, &quot;fMid&quot;, &quot; | &quot;, &quot;Error&quot;, &quot; | &quot;
do while(error &gt; tolerance)
iteration = iteration + 1
xMid = (xLow + xHigh) / 2
fMid = getFunction(xMid)
write(*,20) &quot; | &quot;, iteration, &quot; | &quot;, xLow, &quot; | &quot;, xHigh, &quot; | &quot;, fLow, &quot; | &quot;,
xMid, &quot; | &quot;, fMid, &quot; | &quot;, error, &quot; | &quot;
if(haveOppositeSigns(fLow, fMid)) then
xHigh = xMid
fHigh = fMid
else if(haveOppositeSigns(fHigh, fMid)) then
xLow = xMid
fLow = fMid
else
write(*,*) &quot;Exact Root Found&quot;
write(*,*)
exit
end if
error = (xHigh - xLow) / 2
end do
write(*,*)
write(*,*) &quot;Error (Bisection) = &quot;, error
write(*,*)
10 format(a3, a4, a3, a7, a3, a7, a3, a7, a3, a7, a3, a7, a3, a7, a3)
20 format(a3, i4, a3, f7.2, a3, f7.2, a3, f7.2, a3, f7.2, a3, f7.2, a3, f7.2, a3)
getRootWithBisection = xMid
end function getRootWithBisection
