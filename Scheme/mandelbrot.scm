;; Eigene Implementierung von iota
(define (iota n)
  (let loop ((i 0) (result '()))
    (if (= i n)
        (reverse result)
        (loop (+ i 1) (cons i result)))))

(define (mandelbrot c max-iter)
  (define (mandelbrot-iter z iter)
    (if (or (>= iter max-iter) (> (magnitude z) 2))
        iter
        (mandelbrot-iter (+ (* z z) c) (+ iter 1))))
  (mandelbrot-iter 0 max-iter))

(define (draw-mandelbrot width height x-min x-max y-min y-max max-iter)
  (let* ((x-scale (/ (- x-max x-min) width))
         (y-scale (/ (- y-max y-min) height)))
    (for-each
     (lambda (y)
       (for-each
        (lambda (x)
          (let* ((real (+ x-min (* x-scale x)))
                 (imag (+ y-min (* y-scale y)))
                 (c (+ real (* imag 0+1i)))
                 (m (mandelbrot c max-iter)))
            (if (= m max-iter)
                (display "#")
                (display " "))))
        (iota width))
       (newline))
     (iota height))))

;; Beispielaufruf
(draw-mandelbrot 80 40 -2.0 1.0 -1.0 1.0 100)
