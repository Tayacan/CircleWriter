# Grammar and semantics for Circ

Circ is a language for specifying drawings of pretty circle nonsense. Transpiles
to SVG.

## GRAMMAR

    Drawing ::= Exprs
    
    Expr     ::= Transform | Shape
    Exprs    ::= Expr Exprs | ε
    
    Tranform ::= 'translate' Pos  Block
               | 'rotate'    real Block
    
    Block ::= '{' Exprs '}'
    
    Shape  ::= Circle
             | Dot
             | Arc
             | Triangle
             | Line
    
    Circle   ::= 'circle'   OptPos int
    Dot      ::= 'dot'      OptPos int
    Arc      ::= 'arc'      OptPos int real
    Triangle ::= 'triangle' OptPos int
    Line     ::= 'line'     Pos Pos
    
    Pos    ::= '(' int ',' int ')'
    OptPos ::= ε | Pos

### Whitespace

Whitespace rules can be pretty lax, because all expressions start with a unique
keyword, leaving little room for ambiguity. There must be some sort of whitespace
separating keywords and numbers, but brackets and parentheses need not be
surrounded by whitespace.

## SEMANTICS

The entry point to the grammar is *Drawing*. A drawing is a list of expressions, and
and expression is either a shape, or some transformation applied to a group of
expressions.

Transformations are applied inside out. For example, in the following expression:

    translate (100, 0) {
        circle (0, 0) 30
        translate (0, 30) {
            triangle (0, 0) 10
        }
    }

The innermost transform will be applied first, resulting in a circle at (0, 0) and
a triangle at (0, 30). Only then is the outer transform applied, moving the circle
to (100, 0) and the triangle to (100, 30).

The result is that when we're creating our shapes, we don't have to worry about the
context they're being created in. They only have to be positioned and rotated in
relation to shapes in the same local scope.

An OptPos which is absent will default to (0, 0). So the above example can be written
as:

    translate (100, 0) {
        circle 30
        translate (0, 30) {
            triangle 10
        }
    }

### Shapes

* A circle is defined by a center point and a radius.
* A dot is like a circle, but with black fill color.
* An arc is defined by a center point, a radius, and an angle α. The arc runs along the
  circle defined by the center and radius, from angle 0 to α.
* A triangle is defined by a center point and a side length. By default, it points up.
* A line is defined by its start and end points.
