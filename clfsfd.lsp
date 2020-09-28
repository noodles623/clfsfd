(asdf:load-system :opticl)

(defpackage :clfsfd
  (:use :common-lisp :opticl))

(in-package :clfsfd)

(defun get-pixel-string (img i j)
  (multiple-value-bind (r g b)
      (pixel img i j)
    (coerce (map 'list #'code-char
		 (list r g b)) 'string)))

(defun pixels-equal (img p1 p2)
  (multiple-value-bind (r1 g1 b1) (pixel img 0 p1)
    (multiple-value-bind (r2 g2 b2) (pixel img 0 p2)
      (declare (type (unsigned-byte 8) r1 g1 b1 r2 g2 b2))
      (and (= r1 r2) (= g1 g2) (= b1 b2)))))

(defun get-img-string-list (img)
  (with-image-bounds (height width) img
    (loop for j below (- width 1)
	  for y = (get-pixel-string img 0 j)
	  when (or
		(= 0 j)
		(not (pixels-equal img j (- j 1))))
	    collect y)))

(defun load-img (filename)
    (cond
      ((string-equal (ppcre:scan-to-strings "\[^\.]*$" filename)  "png")
       (read-png-file filename))
      (T (error "Invalid file extension"))))

(defun decode-img (filename)
  (let ((img (load-img filename)))
    (typecase img
      (8-bit-rgb-image
       (locally
	   (declare (type 8-bit-rgb-image img))
	 (format nil "~{~a~}" (get-img-string-list img)))))))
