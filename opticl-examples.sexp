
((:p
  (:smarkup-metadata 
   (:copyright
    "Copyright 2011, Cyrus Harmon. See LICENSE file.")
   (:title "Opticl Examples")
   (:author "Cyrus L. Harmon"))
  (:html-metadata
   (:htmlcss "smarkup.css")
   (:htmlcss "reset.css")))
 
 (:h1 "Opticl Examples")
 
 (:h2 "Preliminaries")
 
 (:p "Let's make (and use) a Common Lisp package for exploring the opticl examples:")
 
 (:lisp
  #q{
  (defpackage #:opticl-examples
    (:use #:cl #:opticl))
  (in-package #:opticl-examples)
  })
 
 (:p "Second, we'll make some utility functions that we'll use for
  finding sample images and figuring out where to place output
  images:")

 (:lisp
  #q{
  (defun example-image (filename)
    (merge-pathnames filename
                     (asdf:component-pathname
                      (reduce #'asdf:find-component
                              '("opticl-examples" "images")))))
  
  (let ((output-directory
         (merge-pathnames "output/"
                          (asdf:component-pathname
                           (asdf:find-system "opticl-examples")))))
    (ensure-directories-exist output-directory)
    (defun output-image (filename)
      (merge-pathnames filename output-directory)))
  })
 
 (:h2 "Making Some (Small) Images")
 
 (:p #q{First, a 8-bit grayscale image:})
 
 (:lisp #q{(defparameter *gray-image* (make-8-bit-gray-image 4 4))})
 
 (:p "Evaluating " (:code "*gray-image* " " yields:"))
 
 (:lisp #q{*gray-image*})
 
 (:p "we see that we have a 2-d array that contains the pixel information for the image.")
 
 (:p #q{Now, an 8-bit RGB image:})
 
 (:lisp #q{(defparameter *rgb-image* (make-8-bit-rgb-image 4 4))})
 
 (:p "Now, when we evalute " (:code "*rgb-image*") 
     " we see that " (:code "*rgb-image*") " contains a 3-d array:")
 
 (:lisp #q{*rgb-image*})
 
 (:h2 "Loading Images from Files")

 (:p "In this simple example, we'll load a TIFF image into the variable "
     (:code "*truck-image*") " and save it as a JPEG file:")

 (:lisp
  #q{
  (defparameter *truck-image* (read-tiff-file (example-image "truck.tiff")))

  (defparameter *truck-jpeg-file*
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out *truck-image*)))
  })

 (:image (:lisp-value #q{*truck-jpeg-file*}))
 
 (:h2 "Resizing an image")

 (:p "To resize the truck image from above, we can do: ")
 
 (:lisp
  #q{
  (defparameter *resized-truck-image*
    (resize-image *truck-image* 200 300))
  
  (defparameter *resized-truck-jpeg-file*
    (let ((out (output-image "resized-truck.jpeg")))
      (write-jpeg-file out *resized-truck-image*)))
  })

 (:image (:lisp-value #q{(namestring *resized-truck-jpeg-file*)}))

 (:h2 "Affine Transformations")

 (:p #q{Resizing an image is a special case of applying an affine
     transformation to an image. An affine transformation can be thought
     of as a composition of zero or more linear operations of rotating,
     scaling, shearing or shifting an image. Each point (y, x) in the
     original image gets mapped to a new point (y', x') in the new
     image. This mapping can be thought of as being represented by two
     functions, both of which have the general form f(y, x) = ay + bx +
     c. There are two functions as one yields the y coordinate and the
     other yields the x coordinate, although both functions take both x
     and y coordinates as arguments. The ay + bx aspect of the
     transformation functions is easily modeled by a 2x2 matrix -- but the
     + c part is a bit tricker. But if we want to represent the
     transformation as a matrix (which we do) and we want to use matrix
     multiplication to yield the new coordinates, we need to employ a
     trick of converting the 2 coordinate value (y, x) into so-called
     homogenous coordinates (y, x, 1) and then multiply this by a 3x3
     matrix in which contains the 2x2 matrix above, along with a 1 on the
     third diagonal position and appropriate values representing the +c
     portions of the above equations in the proper positions such that
     when the coordinated is multiplied by the matrix, the constants are
     added to the appropriate coordinates -- the other 2 remaining values
     become 0. The only trick about this is which values get 0 and which
     get the two constant values? Well, it depends if we are multiplying
     our 3 (remember we converted from 2 to 3 a moment ago, by adding a 1
                     in the third position) coordinates, taken as a row vector, by the 3x3
     matrix, or if we are post-multiplying the 3x3 transformation matrix
     by a 3-element column vector. The two approaches are equivalent, but
     we need to be a little careful as matrix multiplication isn't
     commutative. Fortunately, AB = T(T(B)T(A)), that is A times B is
     equal to the transpose of the transpose of B times the transpose of
     A. The beauty of all of this math is that we can compose linear
     transformations such that the constant transformations are
     premultiplied, and then post-multiply that composed transformation by
     our data (expressed as column vectors). This means that we can
     compose transformations thusly:})

 (:lisp-no-eval
  #q{
  ...
  (let ((composed
         ;; recall that the order of the transformations matter, and
         ;; that the transform that we (conceptually) wish to apply
         ;; first, must be last to be multiplied
         (reduce #'matrix-multiply (reverse (list pre-shift rotate post-shift)))))
    (transform-image img composed))
  ...
  })

 (:p "But picking the appropriate values for the transformation matrix
 can be a bit tricky. To help with the process there is a function
 called " (:code "make-affine-transformation") ", which takes as
 keyword arguments " (:code "y-shift, x-shift, theta, y-scale,
 x-scale, y-shear and x-shear") ". These seven parameters are then
 converted into a 6-parameter 3x3 (recall that three of the values in
 the matrix are taken by the two zeroes and the one in the 3rd
 diagonal position), which can then by passed to
 the " (:code "transform-image" " function."))
 
 (:p "Let's see this in action. First, we'll make a small image to play with:")

 (:lisp
  #q{
  (defparameter *cropped-salad*
    (crop-image (read-jpeg-file (example-image "salad.jpg")) 400 200 600 400))

  (defparameter *cropped-salad-file*
    (write-png-file (output-image "cropped-salad.png") *cropped-salad*))
  })
 
 (:image (:lisp-value #q{*cropped-salad-file*}))

 (:p "Now we'll apply some transformations to it:")
 
 (:lisp
  #q{
  (defparameter *squashed-salad-file*
    (let ((transform (make-affine-transformation :x-scale 1.5d0 :y-scale .75d0)))
      (let ((bigimg
             (transform-image *cropped-salad* transform :interpolate :bilinear)))
        (write-png-file (output-image "salad-big.png") bigimg))))
  })
 
 (:image (:lisp-value #q{*squashed-salad-file*}))
 
 (:p "Next, we'll apply a more radical transformation:")

 (:lisp
  #q{
  (defparameter *salad-trans-file*
    (let ((transform (make-affine-transformation
                      :x-scale 0.7d0 :y-scale 1.1d0
                      :x-shear 1.3d0 :y-shear 1.8d0
                      :theta (* -45.0d0 (/ 180.0d0) pi)
                      :x-shift 40 :y-shift 40)))
      (let ((transimg
             (transform-image *cropped-salad* transform :interpolate :bilinear)))
        (write-png-file (output-image "salad-trans.png") transimg))))
  })

 (:image (:lisp-value #q{*salad-trans-file*}))
 
 (:h2 "Discrete Convolution")

 (:lisp
  #q{
  (defparameter *truck*
         (read-jpeg-file (example-image "truck.jpeg")))
  })
 
 (:h3 "Blur")

 (:lisp
  #q{
  (defparameter *blurred-truck-file*
    (write-png-file
     (output-image "truck-blur.png")
     (blur-image *truck*)))
  })
 
 (:image (:lisp-value #q{*blurred-truck-file*}))
 
 (:h3 "Sharpen")
 
 (:lisp
  #q{
  (defparameter *sharpened-truck-file*
    (write-png-file
     (output-image "truck-sharpen.png")
     (sharpen-image *truck*)))
  })
 
 (:image (:lisp-value #q{*sharpened-truck-file*}))
 
 (:h3 "Custom Convolution Kernels")
 
 (:h2 "Drawing Circles")

 (:lisp
  #q{
  (defun example-circles ()
    (declare (optimize (speed 3) (safety 0)))
    (let ((height 480) (width 640))
      (let ((img (make-8-bit-rgb-image height width)))
        (declare (type 8-bit-rgb-image img))
        (fill-image img 20 20 90)
        (loop for i below 100
           do (let ((y (random height))
                    (x (random width))
                    (radius (random 100))
                    (r (random 256))
                    (g (random 256))
                    (b (random 256))
                    (fill (random 2)))
                (if (plusp fill)
                    (opticl::fill-circle img y x radius r g b)
                    (draw-circle img y x radius r g b))))
        img)))

  (defun write-circle-images ()
    (let ((img (example-circles)))
      (write-png-file (output-image "circles.png") img)))
  
  (defparameter *circles* (write-circle-images))
  })
 
 (:image
  (:lisp-value
   #q{*circles*})))

