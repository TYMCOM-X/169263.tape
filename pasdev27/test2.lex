scanner test2;
alphabet is a, b;
s = (a|b)*;
a a a {};
a+ {};
a b a b {};
a s b {};
(a b | b a)+ {};
end
 