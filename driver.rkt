#lang racket
(require "./DBSCAN.rkt")

;Uncomment to print initial input
;(display step1)
;(newline)

;Uncomment to print similarity matrix
;(display step2)
;(newline)

;Uncomment to print K nearest neighbours for every point
;(display step3)
;(newline)

;Uncomment to print shared neighbour graph
;(display step4)
;(newline)

;Uncomment to print point densities 
;(display step5)
;(newline)

;Uncomment to print core points
;(display step6)
;(newline)


(print "Core points cluster")
(newline)
(display step7)
(newline)

(print "Noise Points")
(newline)
(display step8)
(newline)

(print "Border Points")
(newline)
(display step9)
(newline)

(print "Complete Clusters")
(newline)
(display step10)
