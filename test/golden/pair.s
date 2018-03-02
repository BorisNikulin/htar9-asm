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
add 55 // ra = 55
add 55 // ra = 110
add 18 // ra = 128

mv r1 // r1 = 128
ld r1 // ra = *128
mv r3 // r3 = *128, first array value

add r1 // ra = 128
add 1  // ra = 129
mv r2  // r2 = 129

ld r2  // ra = *129
mv r4  // r4 = *129, second array value

add r3 // ra = *128
dist r4 // ra = distance(*128, *129)

mv r6  // r6 = distance(*128, *129)

// outer loop
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
dist r4 // ra = distance(128 - *129)

mv r5  // r5 = *128 - *129

ba loopCont // need to continue the loop

// increments outer loop for next outer loop iteration
incOuter:

// increment outer loop and branch back to beginning of outer loop
mv ra  // clear ra
add r1 // ra = r1
add 1  // ra = r1 + 1
mv r1 // r1 = r1 + 1

ba outerLoop // branch to outerLoop

// r5 is the current distance
loopCont:

add r5 // ra = r5
min r6 // ra = min(r5, r6)
mv r6  // r6 = new minimum

ba prepInnerLoop // otherwise prep the inner loop without setting min

// increments inner loop for next inner loop iteration
incInner:

// increment inner loop and branch back to beginning of inner loop
mv ra  // clear ra
add r2 // ra = r2
add 1  // ra = r2 + 1
mv r2 // r2 = r2 + 1

ba innerLoop // branch to innerLoop

// inner loop is complete, back to outer loop
exitInnerLoop:

// check if outer loop counter is at 146, meaning that it has completed
add 55 // ra = 55
add 55 // ra = 110
add 36 // ra = 146

sub r1 // ra = 146 - r1, ra = 0 if loop is complete

// if outer loop is not complete, cond reg is set, branch
bcs incOuter // increment outer loop counter

ba exitOuterLoop // otherwise, exit outer loop

// either way, prepare for next loop iteration
prepInnerLoop:

// check if inner loop counter is at 147, meaning that it has completed
add 55 // ra = 55
add 55 // ra = 110
add 37 // ra = 147

sub r2 // ra = 147 - r2, ra = 0 if loop if complete

// if inner loop is not complete, cond reg is set, branch
bcs incInner // increment inner loop counter

ba exitInnerLoop // branch to exitInnerLoop

// Once outer loop is done, finalize output here
exitOuterLoop:

// place minimum distance (r6) into memory location 127
add 55 // ra = 55
add 55 // ra = 110
add 17 // ra = 127

mv r1  // r1 = 127

add r6 // ra = r6
str r1 // *r1 = r6

fin // done
reset // reset PC to 0
