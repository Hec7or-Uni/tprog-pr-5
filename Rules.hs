module Rules where

-- ID_RULE: aaa
-- Rules for: Curva de Koch
aaa :: Char -> String
aaa c
 | c == 'F' = "F+F--F+F"
 | otherwise = [c]

-- ID_RULE: aab
-- Rules for: Curva de Koch cuadrada
aab :: Char -> String
aab c
 | c == 'F' = "F+F-F-F+F"
 | otherwise = [c]

-- ID_RULE: aac
-- Rules for: Koch Snowflake
aac :: Char -> String
aac c
 | c == 'F' = "F-F++F-F"
 | otherwise = [c]

-- ID_RULE: aad
-- Rules for: Koch Anti–Snowflake
aad :: Char -> String
aad c
 | c == 'F' = "F+F--F+F"
 | otherwise = [c]

-- ID_RULE: aae
-- Rules for: Isla de Minkowski
aae :: Char -> String
aae c
 | c == 'F' = "F+F-F-FF+F+F-F"
 | otherwise = [c]

-- ID_RULE: aaf
-- Rules for: Triángulo de Sierpinsky
aaf :: Char -> String
aaf c
 | c == 'F' = "F-G+F+G-F"
 | c == 'G' = "GG"
 | otherwise = [c]

-- ID_RULE: aag
-- Rules for: Sierpinsky Arrowhead
aag :: Char -> String
aag c
 | c == 'F' = "G-F-G"
 | c == 'G' = "F+G+F"
 | otherwise = [c]

-- ID_RULE: aah
-- Rules for: Curva de Hilbert
aah :: Char -> String
aah c
 | c == 'f' = "-g>+f>f+>g-"
 | c == 'g' = "+f>-g>g->f+"
 | otherwise = [c]

-- ID_RULE: aai
-- Rules for: Curva de Gosper
aai :: Char -> String
aai c
 | c == 'F' = "F-G--G+F++FF+G-"
 | c == 'G' = "+F-GG--G-F++F+G"
 | otherwise = [c]