package main

import (
	"image"
	"image/color"
	"image/draw"
	"image/png"
	"log"
	"os"
)

func main() {
	img := image.NewRGBA(image.Rect(0, 0, 100, 100))
	c1 := color.RGBA{255, 0, 0, 255}
	draw.Draw(img, img.Bounds(), &image.Uniform{c1}, image.ZP, draw.Src)
	c2 := color.RGBA{0, 255, 0, 255}
	draw.Draw(img, image.Rect(10, 10, 20, 20), &image.Uniform{c2}, image.ZP, draw.Src)
	writePNG(img, "/tmp/golang-draw-test.png")
}

func writePNG(img image.Image, filename string) {
	f, err := os.Create(filename)
	if err != nil {
		log.Fatalln("Couldn't create file.", err)
	}
	defer f.Close()
	err = png.Encode(f, img)
	if err != nil {
		log.Fatalln("Error encoding image.", err)
	}
}
