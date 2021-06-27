import SVG
import Turtle
import Rules
import Data.Char

tplot :: Turtle -> String -> [Position]
tplot (paso,giro,(x,y),orn) [] = [(x,y)]
tplot (paso,giro,(x,y),orn) (s:ss)
 | s == '>' = (x,y) : tplot(moveTurtle (paso,giro,(x,y),orn) Forward) ss
 | s == '+' = (x,y) : tplot(moveTurtle (paso,giro,(x,y),orn) TurnRight) ss
 | s == '-' = (x,y) : tplot(moveTurtle (paso,giro,(x,y),orn) TurnLeft) ss
 | isUpper s = (x,y) : tplot(moveTurtle (paso,giro,(x,y),orn) Forward) ss
 | otherwise = tplot(moveTurtle (paso,giro,(x,y),orn) Forward) ss

norm :: (Char -> String) -> String -> String
norm _ [] = []
norm r (s:ss) = r s ++ norm r ss

lsystem :: (Char -> String) -> String -> Int -> String
lsystem r c i
 | i == 0 = c
 | i >= 1 = lsystem r (norm r c) (i-1)
 | otherwise = []

main = do
 -- Parte 1 | Gráficos de Tortuga
 let
  triangulo = tplot (1,60,(0.5,0),0) ">++>++>"
  cuadrado  = tplot (1,90,(0,0),0) ">+>+>+>"
  circulo   = tplot (1/90,1,(0.5,0),0) ">+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>+>"

 savesvg "triangulo" triangulo
 savesvg "cuadrado" cuadrado
 savesvg "circulo" circulo

 -- Parte 2 | Sistemas de Lindenmayer
 let
  ck  = tplot (1,60,(0,0),0) (lsystem aaa "F" 3)
  ckc = tplot (1,90,(0,0),0) (lsystem aab "F" 4)
  ks  = tplot (1,60,(0,0),0) (lsystem aac "F++F++F" 3)
  kas = tplot (1,60,(0,0),0) (lsystem aad "F++F++F" 6)
  im  = tplot (1,90,(0,0),0) (lsystem aae "F+F+F+F" 2)
  ts  = tplot (1,120,(0,0),0) (lsystem aaf "F-G-G" 3)
  sa  = tplot (1,60,(0,0),0) (lsystem aag "F" 6)
  ch  = tplot (1,90,(0,0),0) (lsystem aah "f" 4)
  cg  = tplot (1,60,(0,0),0) (lsystem aai "F" 3)

 savesvg "Curva de Koch" ck
 savesvg "Curva de Koch cuadrada" ckc
 savesvg "Koch Snowflake" ks
 savesvg "Koch Anti–Snowflake" kas
 savesvg "Isla de Minkowski" im
 savesvg "Triángulo de Sierpinsky" ts
 savesvg "Sierpinsky Arrowhead" sa
 savesvg "Curva de Hilbert" ch
 savesvg "Curva de Gosper" cg