////////////////////////////////////////////////////////////
// r1 = current str in arr
// r2 = 4 bit comp string
// r3 = memory location of current array index
// r4 = counter for # left shifts in str
// r5 = masked str
// r6 = # matches
////////////////////////////////////////////////////////////

mv ra // make sure ra is cleared

// load comp from memory at address 6, place in r2
add 6 // ra = 6
mv r2 // r2 = 6
ld r2 // ra = *r2, which is whatever is at memory location 6
mv r2 // r2 = bits at mem location 6, the comp string

// mask comp in r2 to get lower 4 bits
add 15 // ra = 15, which is 00001111
and r2 // ra = 00001111 & r2, so the lower 4 bits of r2
mv r2  // store lower 4 bits of comp in r2

// store 32 in r3, the starting memory index of the array
add 31 // ra = 31
add 1  // ra = 32
mv r3  // r3 = 32

// r6 is # matches, starts at 0
mv r6

// label to iterate through all 64 bytes in array
arrayLoop:

// load current str from array into r1
ld r3 // ra = *r3, where r3 is current memory index
mv r1 // r1 = ra

// clear r4, the # str shifts
mv r4 // r4 = 0

// label to iterate through str, pattern matching for comp
strLoop:

// mask str to get lower 4 bits, move into r5
add 15 // ra = 15, which is 00001111
and r1 // ra = ra & r1, ra = lower 4 bits of r1
mv r5  // r5 = ra

// compare lower 4 bits of str with comp (r5 and r2)
add r5 // ra = 0 + r5 = r5
sub r2 // ra = r5 - r2

// if r5 != r2, then ra != 0, condition register set
bcs noMatch // branch to handle match failure

// if r5 == r2, then ra = 0, condition register unset, so above
// branch is false, goes here instead
add r6 // ra = r6
add 1  // ra = r6 + 1
mv r6  // r6 = r6 + 1, increment counter for matches

ba endStrLoop // branch to end of strLoop

// prepArrayLoop increments the array counter before branching
// back to the arrayLoop for another iteration
prepArrayLoop:

mv ra  // clear ra
add r3 // ra = r3
add 1  // ra = r3 + 1
mv r3  // r3 = r3 + 1, counter incremented

ba arrayLoop // branch to arrayLoop

// shifts str and increments shift counter for next strLoop
prepStrLoop:

// left shift str
mv ra  // clear ra
add r1 // ra = r1
lshft 1 // ra = r1 >> 1
mv r1  // r1 = r1 >> 1

// increment shift counter
add r4 // ra = r4
add 1  // ra = r4 + 1
mv r4  // r4 = r4 + 1, counter incremented

ba strLoop // branch to strLoop

// if lower 4 bits of comp and str don't match, handle that here
noMatch:

// check if r4 == 4, meaning that the current str has been
// shifted 4 times and is done being checked
mv ra  // ra = 0
add 4  // ra = 4
sub r4 // ra = 4 - r4

// if r4 != 4, then ra != 0, cond reg is set
bcs prepStrLoop // branch to where r1 and r4 are updated

// if r4 == ra, then ra = 0, above branch is skipped
// end of strLoop is where byte location (r3) increments and
// either branch back to arrayLoop or to endArrayLoop
endStrLoop:

// check if array counter is 96, since that means the entire
// array from mem addresses 32 through 96 have been checked
add 31 // ra = 31
add 31 // ra = 31 + 31 = 62
add 31 // ra = 62 + 31 = 93
add 3  // ra = 93 +  3 = 96
sub r3 // ra = 96 - r3

// if r3 != 96, then need to branch to update r3
bcs prepArrayLoop

// if r3 == 96, above branch is skipped, go to end of arrayLoop
// endArrayLoop occurs when all 64 bytes have been checked
endArrayLoop:

// once array is done being checked, save counter in r6 to mem
// location 7
add 7  // ra = 7
mv r1  // r1 = 7
add r6 // ra = r6, where r6 is # matches
str r1 // store value in ra at *r1, which is mem location 7

fin // done
