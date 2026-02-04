#!/bin/sh

docker run --rm -t -v $(pwd):/slides ghcr.io/astefanutti/decktape \
  reveal \
  --progress \
  slides/un_intro_slides.html \
  slides/un_intro_slides.pdf

docker run --rm -t -v $(pwd):/slides ghcr.io/astefanutti/decktape \
  reveal \
  --progress \
  slides/un_A_slides.html \
  slides/un_A_slides.pdf

docker run --rm -t -v $(pwd):/slides ghcr.io/astefanutti/decktape \
  reveal \
  --progress \
  slides/un_B_slides.html \
  slides/un_B_slides.pdf
  
docker run --rm -t -v $(pwd):/slides ghcr.io/astefanutti/decktape \
  reveal \
  --progress \
  slides/un_C_slides.html \
  slides/un_C_slides.pdf
  
docker run --rm -t -v $(pwd):/slides ghcr.io/astefanutti/decktape \
  reveal \
  --progress \
  slides/un_D_slides.html \
  slides/un_D_slides.pdf
  
docker run --rm -t -v $(pwd):/slides ghcr.io/astefanutti/decktape \
  reveal \
  --progress \
  slides/un_E_slides.html \
  slides/un_E_slides.pdf
  
docker run --rm -t -v $(pwd):/slides ghcr.io/astefanutti/decktape \
  reveal \
  --progress \
  slides/un_F_slides.html \
  slides/un_F_slides.pdf