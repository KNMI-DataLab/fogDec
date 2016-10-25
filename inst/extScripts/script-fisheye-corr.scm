(define (rectify-image filename
		  main-adjust
                  edge-adjust
		  rescale)
   (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
          (drawable (car (gimp-image-get-active-layer image))))
     (plug-in-lens-distortion RUN-NONINTERACTIVE
                       image drawable 0 0 main-adjust edge-adjust rescale 0)
     (gimp-file-save RUN-NONINTERACTIVE image drawable filename filename)
     (gimp-image-delete image)))
