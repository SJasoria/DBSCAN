#lang racket
(provide (all-defined-out))
(define inputfile (vector-ref (current-command-line-arguments) 0))
(define raw-data (file->list inputfile))

;precision correction
(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

;STEP 1: Populate dataset from input file

(define N (car raw-data))
(define raw-data1 (cdr raw-data))
(define D (car raw-data1))
(define raw-data2 (cdr raw-data1))
(define K (car raw-data2))
(define raw-data3 (cdr raw-data2))
(define E (car raw-data3))
(define raw-data4 (cdr raw-data3))
(define MinPts (car raw-data4))
(define raw-data5 (cdr raw-data4))

;function to map with element index
(define (maptoindex ls count result)
	(cond
	((null? ls) result)
	(else (maptoindex (cdr ls) (+ count 1) (append result (list (cons count (list(car ls)))))))))

(define (creatematrix ls matrix)
	(cond
		((null? ls) matrix)
		(else (creatematrix (drop ls D) (append matrix (list (take ls D)))))))

(define initialmatrix (creatematrix raw-data5 '()))
(define  step1 (maptoindex initialmatrix '1 '()))

;STEP 2: Calculate similarity matrix

;function to calculate distance between two points
(define (calculatedistance ls1 ls2 distance)
	(cond
		((null? ls1) (sqrt distance))
		(else (calculatedistance 
				(drop ls1 1) (drop ls2 1) (+ distance (expt (- (car ls1) (car ls2)) 2))))))

;function to calulate distance of one vector with the rest
(define (calculatedistanceall ls matrix count1 count2 result)
	(cond
		((null? matrix) (maptoindex result '1 '()))
		((equal? count1 count2) (calculatedistanceall ls (cdr matrix) count1 (+ count2 1)(append result '(+inf.0))))
		(else (calculatedistanceall ls (cdr matrix) count1 (+ count2 1) (append result (list (calculatedistance ls (car matrix) '0)))))))

;function to calculate distance for all vectors
(define (finddistance matrix1 matrix2 count result)
	(cond
		((null? matrix1) result)
		(else (finddistance (cdr matrix1) matrix2 (+ count 1) (append result (list (calculatedistanceall (car matrix1) matrix2 count '1 '())))))))

(define similaritymatrix (finddistance initialmatrix initialmatrix  '1 '()))
(define step2 (modify_precision similaritymatrix))

;STEP 3: Find the K nearest neighbour for every data point

(define (knn matrix result)
	(cond
		((null? matrix) result)
		(else (knn (cdr matrix) (append result (list (sort (car matrix) #:key cadr <)))))))

(define (selectlowestfive matrix result)
	(cond
		((null? matrix) result)
		(else (selectlowestfive (cdr matrix) (append  result (list (sort (take (map (lambda(x)(car x)) (car matrix)) K) < )))))))

(define step3 (selectlowestfive (knn step2 '()) '()))

;STEP 4: Calculate shared neighbour graph

(define (common l1 l2 count)
	(cond
	((null? l1) count)
	((null? l2) count)
	((eq? (car l1) (car l2)) (common (cdr l1) (cdr l2) (+ count 1)))
	((< (car l1) (car l2)) (common (cdr l1) l2 count))
	((> (car l1) (car l2)) (common l1 (cdr l2) count))))

(define (sim ls matrix count1 count2 result)
	(cond
		((null? matrix) (sort result #:key cadr >))
		((and (member count2 ls) (member count1 (car matrix))) 
			(sim ls (cdr matrix) count1 (+ count2 1) (append result (list (list count2 (common ls (car matrix) '0))))))
		(else (sim ls (cdr matrix) count1 (+ count2 1) (append result '())))))

(define (sngraph matrix1 matrix2 count graph)
	(cond
		((null? matrix1) graph)
		(else (sngraph (cdr matrix1) matrix2 (+ count 1) (append graph (list (sim (car matrix1) matrix2 count 1 '())))))))

(define step4 (sngraph step3 step3 1 '()))

;STEP 5: Evaluate point density

(define (countE ls count)
	(cond
	((null? ls) count)
	((>= (cadar ls) E) (countE (cdr ls) (+ count 1)))
	(else (countE (cdr ls) count))))

(define (pointdensity matrix result)
	(cond
	((null? matrix) result)
	(else (pointdensity (cdr matrix) (append result (list (countE (car matrix) '0)))))))

(define step5 (pointdensity step4 '()))

;STEP 6: Identify core points

(define (corepoints ls count result)
	(cond
	((null? ls) result)
	((>= (car ls) MinPts) (corepoints (cdr ls) (+ count 1) (append result (list count))))
	(else (corepoints (cdr ls) (+ count 1) result))))

(define step6 (corepoints step5 '1 '()))

;STEP 7: Cluster the core points

(define (weight sng p2)
	(cond
		((null? sng) '0)
		((eq? p2 (caar sng)) (cadar sng))
		(else (weight (cdr sng) p2))
))

(define (pointstocheck ls p result)
	(cond
		((null? ls) result)
		((>= (weight (list-ref step4 (- p 1)) (car ls)) E) (pointstocheck (cdr ls) p (append result (list (car ls)))))
		(else (pointstocheck (cdr ls) p result))))

(define (edgecheck ls1 wts result)
	(cond
		((null? ls1) result)
		((and (member (car ls1) step6) (>= (weight (list-ref step4 (- wts 1)) (car ls1)) E )) (edgecheck (cdr ls1) wts (append result (list (car ls1)))))
		(else (edgecheck (cdr ls1) wts result))))

(define (updatecluster v ptc matrix cluster)
	(cond
		((null? ptc) (sort (remove-duplicates cluster) <))
		((and (member (car ptc) step6)  (not (member (car ptc) cluster)) (not (member (car ptc) v))) 
			(updatecluster 
				(append v (list (car ptc))) 
				(cdr (append ptc (pointstocheck (list-ref matrix (- (car ptc) 1)) (car ptc)'())))  
				matrix
				(append cluster (list (car ptc)) (edgecheck (list-ref matrix (- (car ptc) 1)) (car ptc) '()))))
		((and (member (car ptc) step6)  (member (car ptc) cluster) (not (member (car ptc) v))) 
			(updatecluster 
				(append v (list (car ptc))) 
				(cdr (append ptc (pointstocheck (list-ref matrix (- (car ptc) 1)) (car ptc)'())))
				matrix
				(append cluster (edgecheck (list-ref matrix (- (car ptc) 1)) (car ptc) '()))))
		(else (updatecluster v (cdr ptc) matrix  cluster))))

(define (dfs ls1  matrix  cluster)
	(cond
		((null? ls1) (maptoindex cluster '1 '()))
		((not (member (car ls1) (flatten cluster))) (dfs (cdr ls1) matrix (append cluster (list (updatecluster '() (list (car ls1)) matrix '())))))
		(else (dfs (cdr ls1) matrix cluster))))

(define step7 (dfs step6 step3 '()))

;STEP 8: Identify noise points

(define (noisepoints ls count result)
	(cond
		((null? ls) result)
		((eq? '0 (car ls)) (noisepoints (cdr ls) (+ count 1) (append result (list count))))
		(else (noisepoints (cdr ls) (+ count 1) result))))

(define step8 (noisepoints step5 '1 '()))

;STEP 9: Identify border points

(define (borderpoints ls1 ls2)
	(cond
		((null? ls2) ls1)
		(else (borderpoints (remove (car ls2) ls1) (cdr ls2)))))

(define step9 (borderpoints (build-list N (lambda (x) (+ x 1))) (append step6 step8)))


;STEP 10: Add border points to core cluster

(define (sim2 p ls matrix result)
	(cond
		((null? ls) (list (sort result #:key cadr >)))
		(else (sim2 
				p 
				(cdr ls) 
				matrix 
				(append result (list (list (car ls) (common (list-ref matrix (- p  1)) (list-ref matrix (- (car ls) 1)) '0))))))))

(define (similarity ls1 ls2 matrix result)
	(cond 
		((null? ls1) result)
		(else (similarity (cdr ls1) ls2 matrix (append result (sim2 (car ls1) ls2 matrix '()))))))


(define sim-mat (similarity step9 step6 step3 '()))

(define (completecluster ls matrix cluster)
	(cond
		((null? ls) cluster)
		(else (completecluster (cdr ls) (cdr matrix) (findcluster (car ls) (car matrix) cluster cluster) ))))


(define (findcluster p ls cluster completecluster)
	(cond
		((member  (caar ls) (cadar cluster)) (append (remove (car cluster) completecluster) (list (cons (caar cluster) (list (sort (cons p (cadar cluster)) <))))))
		(else (findcluster p ls (cdr cluster) completecluster))))

(define step10 (sort (completecluster step9 sim-mat step7) #:key car <))
