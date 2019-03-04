////////////////////////////////////////////////////////////
// r1 = multiplicand (high)
// r2 = multiplicand (low)
// r3 = multiplier
// r4 = result (high)
// r5 = result (low)
// r6 = counter
////////////////////////////////////////////////////////////

// clear high bits of multiplicand

mv ra // ensure ra is clear
mv r1 // r1 = 0
mv r4 // r4 = 0
mv r5 // r5 = 0

// populate low 8 bits of multiplicand

// r1 = *(1)

add 1 // memory address 1
mv r2 // store in r2
ld r2 // ra = *r2
mv r2 // r2 = ra

// populate multiplier

// r3 = *(2)

add 2 // memory address 2
mv r3 // store in r3
ld r3 // ra = *r3
mv r3 // r3 = ra

// init counter at 1
add 1
mv r6 // r6 = 1

////////////////////////////////////////////////////////////
// first multiplication subroutine
////////////////////////////////////////////////////////////

mult1:

// r6 & r3
add r3 // move multiplier into ra
and r6 // ra = ra & r6

//if(r6 & r3 == 0), skip addition step
bcs additionDone1

// r5 = r5 + r2
mv ra // clear ra first
add r5
add r2
mv r5

//if no overflow, skip add to high
bcs 4

// r4 = r4 + 1
add r4
add 1
mv r4

// r4 = r4 + r1
add r4
add r1
mv r4

additionDone1:

// r1 = r1 << 1
mv ra
add r1
lshft 1
mv r1

// r1 = r1 + ((r2 & (1 << 7)) >> 7) - this is essentially a left-shift from r2 into r1
add 1
lshft 7
and r2
rshft 7
add r1
mv r1

// r2 = r2 << 1
add r2
lshft 1
mv r2

// r6 = r6 << 1
add r6
lshft 1
mv r6

// if r6 != 0, branch back to beginning
bcs mult1

// move result of first multiplication into multiplicand, clear results

add r4
mv r1 // r1 = r4
add r5
mv r2 // r2 = r5
mv r4 // r4 = 0
mv r5 // r5 = 0

// populate multiplier with 3rd byte

add 3 // memory address 3
mv r3 // store in r3
ld r3 // ra = *r3
mv r3 // r3 = ra

// reset counter to 1
add 1
mv r6

////////////////////////////////////////////////////////////
// second multiplication subroutine
////////////////////////////////////////////////////////////

mult2:

// r6 & r3
add r3 // move multiplier into ra
and r6 // ra = ra & r6

//if(r6 & r3 == 0), skip addition step
bcs additionDone2

// r5 = r5 + r2
mv ra // clear ra first
add r5
add r2
mv r5

//if no overflow, skip add to high
bcs 4

// r4 = r4 + 1
add r4
add 1
mv r4

// r4 = r4 + r1
add r4
add r1
mv r4

additionDone2:

// r1 = r1 << 1
mv ra
add r1
lshft 1
mv r1

// r1 = r1 + ((r2 & (1 << 7)) >> 7) - this is essentially a left-shift from r2 into r1
add 1
lshft 7
and r2
rshft 7
add r1
mv r1

// r2 = r2 << 1
add r2
lshft 1
mv r2

// r6 = r6 << 1
add r6
lshft 1
mv r6

// if r6 != 0, branch back to beginning
bcs mult2

////////////////////////////////////////////////////////////
// store results in memory
// r1 = result address
// r4 = result (high bits)
// r5 = result (low bits)
////////////////////////////////////////////////////////////

// populate result address (high)

add 4 // memory address 4 (high bit storage)
mv r1 // store in r1

add r4 // put result into ra
str r1 // *r1 = ra (store high results)

// populate result address (low)

mv ra
add 5 // memory address 5 (low bit storage)
mv r1 // store in r1

add r5 // put result into ra
str r1 // *r1 = ra

fin // done
