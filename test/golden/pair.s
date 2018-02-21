////////////////////////////////////////////////////////////
// r1 = outer loop counter
// r2 = inner loop counter
// r3 = stores outer number for comparison
// r4 = stores inner number for comparison
// r5 = current distance
// r6 = current minimum distance
////////////////////////////////////////////////////////////

mv ra // make sure ra is cleared

// set min distance to abs(arr[0] - arr[1])
add 31 // ra = 31
add 31 // ra = 62
add 31 // ra = 93
add 31 // ra = 124
add 4  // ra = 128

mv r1 // r1 = 128
ld r1 // ra = *128
mv r3 // r3 = *128, first array value

add r1 // ra = 128
add 1  // ra = 129
mv r2  // r2 = 129

ld r2  // ra = *129
mv r4  // r4 = *129, second array value

add r3 // ra = *128
sub r4 // ra = *128 - *129

mv r5  // r5 = *128 - *129

// check if current dist is negative, if it is, need to invert it
add r5 // ra = curr dist
lshft 7 // shft ra 7 places

// if ra was negative, then the sign bit would be 1,
// cond reg is set because shift result is not zero
bcs negativeDist1 // go to resolve negative dist

//otherwise, branch to continue initialization
ba initCont // branch to continue initialization

// subroutine that handles negative dist in 1st comparison
negativeDist1:

// negate dist by subtracting it from 0
mv ra  // clear ra
sub r5 // ra = 0 - r5 = -r5
mv r5  // r5 has been negated

// now that the min distance is positive, set it as current min
initCont:

add r5 // ra = r5 = current distance
mv r6  // r5 = first distance, which is the current minimum distance

// jump to begin outer loop
ba outerLoop

// subroutine that handles negative dist in loop comparison
negativeDist2:

// negate dist by subtracting it from 0
mv ra  // clear ra
sub r5 // ra = 0 - r5 = -r5
mv r5  // r5 has been negated

ba loopCont // branch to loopCont

// loop for comparisons, goes from 128-146, counter maintained in r1
outerLoop:

// initialize inner loop counter (r2) at r1 + 1
add r1 // ra = r1
add 1  // ra = r1 + 1
mv r2  // r2 = r1 + 1

// load array at index r1 into r3 for comparisons
ld r1  // ra = *r1
mv r3  // r3 = *r1

// inner loop goes from r1+1 to 147, counter maintained in r2
innerLoop:

// load array at index r2 into r4 for comparisons
ld r2 // ra = *r2
mv r4 // r4 = *r2

// compare r3 and r4 to get their distance
add r3 // ra = *128
sub r4 // ra = *128 - *129

mv r5  // r5 = *128 - *129

// check if current dist is negative, if it is, need to reverse it
add r5 // ra = curr dist
lshft 7 // shift ra 7 to the left

// if ra was negative, then the sign bit would be 1,
// cond reg is set because shift result is not zero
bcs negativeDist2 // go to resolve negative dist

ba loopCont // need to continue the loop

// increments outer loop for next outer loop iteration
incOuter:

// increment inner loop and branch bck to beginning of inner loop
mv ra  // clear ra
add r1 // ra = r1
add 1  // ra = r1 + 1
mv r1 // r1 = r1 + 1

ba outerLoop // branch to outerLoop

// otherwise, ra was positive, r5 is the current distance
loopCont:

// compare current distance r5 and current minimum distance r6
add r5 // ra = current dist
sub r6 // ra = r5 - r6

// shift ra to see if it is negative
lshft 7 // shift ra 7 to the left

// if ra was negative, sign bit is 1, shifted ra will be nonzero,
// setting the conditional register
bcs newMin // if r5 is lower than r6, it is the new min

ba prepInnerLoop // otherwise prep the inner loop without setting min

// increments inner loop for next inner loop iteration
incInner:

// increment inner loop and branch bck to beginning of inner loop
mv ra  // clear ra
add r2 // ra = r2
add 1  // ra = r2 + 1
mv r2 // r2 = r2 + 1

ba innerLoop // branch to innerLoop

// inner loop is complete, back to outer loop
exitInnerLoop:

// check if inner loop counter is at 146, meaning that it has completed
add 31 // ra = 31
add 31 // ra = 62
add 31 // ra = 93
add 31 // ra = 124
add 22 // ra = 146

sub r1 // ra = 146 - r1, ra = 0 if loop if complete

// if outer loop is not complete, cond reg is set, branch
bcs incOuter // increment outer loop counter

ba exitOuterLoop // otherwise, exit outer loop

// either way, prepare for next loop iteration
prepInnerLoop:

// check if inner loop counter is at 147, meaning that it has completed
add 31 // ra = 31
add 31 // ra = 62
add 31 // ra = 93
add 31 // ra = 124
add 23 // ra = 147

sub r2 // ra = 147 - r2, ra = 0 if loop if complete

// if inner loop is not complete, cond reg is set, branch
bcs incInner // increment inner loop counter

ba exitInnerLoop // branch to exitInnerLoop

// if a new minimum distance as been found, set that here, then return to loop
newMin:

// r5 is new minimum distance, move it to r6
mv ra  // clear ra
add r5 // ra = r5
mv r6  // r6 = r5

ba prepInnerLoop // branch to prepInnerLoop

// Once outer loop is done, finalize output here
exitOuterLoop:

// place minimum distance (r6) into memory location 127
add 31 // ra = 31
add 31 // ra = 62
add 31 // ra = 93
add 31 // ra = 124
add 3  // ra = 127

mv r1  // r1 = 127

add r6 // ra = r6
str r1 // *r1 = r6

fin // done
