

// Precedence is LEFT TO RIGHT NO MATTER WHAT
// SuperCollider does not use traditional mathematical operator precedence.

2 + 5 * 100 // 700, same as (2 + 5) * 100.
2 + (5 * 100) // 502.


//
// Literals
// https://doc.sccode.org/Reference/Literals.html
//

// Integers

-13
666

0xa
0x29a
0x29A

// Floats
0.39
1.2e4

// pi can be used as a suffix
pi
1pi
2pi
3pi
100pi
4.5pi
0pi
-pi
-2pi

// inf for infinity. Useful for repeats.
1 < inf
inf == inf
1 > inf

// Alternate bases (radix)
16rF // base 15
16ra9
2r10
2r10000000
2r11111111

// Scale (music) degrees
2s == 2.1 // scale degree two, sharp
2b == 1.9 // scale degree two, flat
2ss // double sharp
2bb // double flat

// up to four
2ssss
2bbbb

// accidentals can specify cents deviation up to 499 cents
2b50
2s499

// Characters

$A
$B
$.

// escape characters
$\t
$\n
$\\

// Symbols

'x'
\x
'x' === 'x'
'x' === \x
\x === \x

'x' == 'x'
'x' == \x
\x == \x

'aiff'
'\'symbols_within_a_symbols\''
\not really a symbol // illegal

// Strings
"x" === "x"
"x" === \x
"x" == \x
"x" == "x"

// automatic concatenation
"st" "ri" "ng"

"test".size

// multiline
(
a = "st
ri
ng
";
a.size;
)


// Arrays

// array of literals
#[1, 2, 'abc', "def", 4]
#[foo, bar] // an array of symbols
[foo, bar] // only legal if foo and bar have been declared as variables

#[[1,2,3], [4,5,6]]

(
// build a table of note names
var table = ();
value {
    var semitones = [0, 2, 4, 5, 7, 9, 11];
    var naturalNoteNames = ["c", "d", "e", "f", "g", "a", "b"];

    (0..9).do {|o|
        naturalNoteNames.do {|c, i|
            var n = (o + 1) * 12 + semitones[i];
            table[(c ++ o).asSymbol] = n;
            table[(c ++ "s"  ++ o).asSymbol] = n + 1;
            table[(c ++ "ss" ++ o).asSymbol] = n + 2;
            table[(c ++ "b"  ++ o).asSymbol] = n - 1;
            table[(c ++ "bb" ++ o).asSymbol] = n - 2;
        };
    };
};

// translate note names to midi keys
table.atAll(#[c4, e4, gs4, c5, e5, gs5, c6])
)


