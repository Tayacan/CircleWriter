import Circles

main :: IO ()
main = writeFile "examples/foo.svg" $ svg $ foo ++ bar

foo = [ circle 100 (Point 0 0)
      , arc 90 (Point 0 0) (pi/2) pi
      , circle 80 (Point 0 0)
      , arc 70 (Point 0 0) (pi/2) 0
      , circle 60 (Point 0 0)
      , arc 50 (Point 0 0) 0 pi
      , circle 40 (Point 0 0)
      , arc 30 (Point 0 0) pi (pi/2)
      , line (Point 0 100) (Point 0 90)
      , line (Point (-90) 0) (Point (-70) 0)
      , line (Point 0 70) (Point 0 60)
      , line (Point 70 0) (Point 50 0)
      , line (Point (-50) 0) (Point (-30) 0)
      , line (Point 0 30) (Point 0 0)
      , circle 5 (Point 0 100)
      , circle 5 (Point (-80) 0)
      , circle 5 (Point 0 60)
      , circle 5 (Point 60 0)
      , circle 5 (Point (-40) 0)
      , dot 10 (Point 0 0)
      ]

bar = [ circle 100 (Point 250 0)
      , triangle 200 (Point 250 0) (pi/2)
      , line (Point 250 0) (Point 250 115)
      , line (Point 250 0) (Point 352 (-58))
      , line (Point 250 0) (Point 149 (-58))
      , triangle 200 (Point 250 0) (-pi/2)
      , circle 58 (Point 250 0)
      ]
