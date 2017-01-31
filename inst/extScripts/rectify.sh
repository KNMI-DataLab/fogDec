#!/bin/bash
# rectify and cut images
	

for fileImage in $(find /nobackup/users/pagani/TwenteRect -type f -name "*.jpg"); do
                echo item: $fileImage
		#TARGET="'(rectify-image \""${fileImage}"\"-80.0-5.0-31.0)'-b'(gimp-quit 0)'"
		#echo $TARGET
		gimp -i -b '(rectify-image "'${fileImage}"\" -80.0 -5.0 -31.0)'-b'(gimp-quit 0)'"
#-b '(gimp-quit 0)'
		mogrify -crop 553x883+387+38 -type TrueColor $fileImage
            done
            



#gimp -i -b '(rectify-image "/usr/people/pagani/temp/pics/testFish.jpg" -80.0 -5.0 -31.0)' -b '(gimp-quit 0)'
#mogrify -crop 553x883+387+38 /usr/people/pagani/temp/pics/testFish.jpg
