// Demonstrates syntax equivalences

// In the examples below, (1..10) is a shortcut for series(1, nil 10).
(1..10);
series(1, nil, 10);

// new argument syntax
(1..10).collect({|n| n.squared }); // receiver syntax
collect((1..10), {|n| n.squared}); // function call syntax
(1..10).collect { |n| n.squared }; // receiver syntax with trailing function arg
collect((1..10)) { |n| n.squared }; // function call syntax with trailing function arg
(1..10) collect: {|n| n.squared} // binary operator syntax

// old argument syntax
(1..10).collect({arg n; n.squared }); // receiver syntax
collect((1..10), {arg n; n.squared}); // function call syntax
(1..10).collect { arg n; n.squared }; // receiver syntax with trailing function arg
collect((1..10)) { arg n; n.squared }; // function call syntax with trailing function arg
(1..10) collect: {arg n; n.squared} // binary operator syntax

// partial application syntax
(1..10).collect(_.squared); // receiver syntax
collect((1..10),_.squared); // function call syntax
(1..10) collect: _.squared; // binary operator syntax

