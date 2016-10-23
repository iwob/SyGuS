; seven.sl
; Synthesize x * 7 mod 10 


(set-logic LIA)

(synth-fun f ((x Int)) Int
    ((Start Int (
                 x
                 (Constant Int) 
                 (+ Start Start)
                 (- Start Start)
                 (* Start Start)
                 (div Start Start)
                 (mod Start Start)
                 (ite StartBool Start Start)
                 (let (( z Int Start )) Start )))
     (StartBool Bool ((and StartBool StartBool)
                      (or  StartBool StartBool)
                      (not StartBool)
                      (<=  Start Start)
                      (=   Start Start)
                      (>=  Start Start)))))

(declare-var x Int)

(constraint (= (f x) (f (+ x 10))))
(constraint (= (f 1) 7))
(constraint (= (f 2) (- (f 1) 3)))
(constraint (= (f 3) (- (f 2) 3)))
(constraint (= (f 4) (* (f 3) 8)))
(constraint (= (f 5) (- (f 4) 3)))
(constraint (= (f 6) (- (f 5) 3)))
(constraint (= (f 7) (+ (* (f 6) 4) 1)))
(constraint (= (f 8) (* (div (f 7) 3) 2)))
(constraint (= (f 9) (div (f 8) 2)))
(constraint (= (f 0) (- (f 9) (f 9))))

(check-synth)

