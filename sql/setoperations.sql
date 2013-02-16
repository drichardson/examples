select x from theNullSet;
select x from naturalNumbers;
select x from primeNumbers;

select nn.x as join1 from theNullSet ns join naturalNumbers nn on ns.x = nn.x;
select nn.x as join2 from primeNumbers pn join naturalNumbers nn on pn.x = nn.x;

\echo Union will remove duplicates
select x as union1 from primeNumbers
union
select x from naturalNumbers;

\echo Union All will NOT remove duplicates
select x from primeNumbers
union all
select x from naturalNumbers;

\echo Interset removes duplicates
select x from primeNumbers
intersect
select x from naturalNumbers;

\echo ERROR - DOESN'T APPEAR TO BE WORKING Interset All does NOT remove duplicates
select x from primeNumbers
intersect all
select x from naturalNumbers;

\echo Should be null
select x from primeNumbers
except
select x from naturalNumbers;

\echo Should be non-prime natural numbers
select x from naturalNumbers
except
select x from primeNumbers;

\echo Except removes duplicates
select x from primeNumbers
except
select x from naturalNumbers;

\echo Limit to 3 natural numbers {1 2 3}.
select x from naturalNumbers limit 3;

\echo Limit to 4 natural numbers skipping the first 2 {3 4 5 6}
select x from naturalNumbers limit 4 offset 2;

\echo Sum the first 10 natural numbers (result should be 55)
select sum(x) from naturalNumbers where x <= 10;

\echo Find combinations of numbers that equal 13.
select nn.x, nn2.x from naturalNumbers nn cross join
(select x from naturalNumbers) as nn2
where nn.x + nn2.x = 13;
