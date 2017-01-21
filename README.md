A tool for flattening multiple images into a single image. There are three modes: the first removes differences, the second combines differences and the third averages them.

These three images of an overly colourful futon can be combined using the three different modes

![Alt text](/example/input/bear1.jpg?raw=true)
![Alt text](/example/input/bear2.jpg?raw=true)
![Alt text](/example/input/bear3.jpg?raw=true)

Using 'remove' mode `image-flatten -r -i example/input -o example/remove.jpg`:
![Alt text](/example/remove.jpg?raw=true)

Using 'combine' mode `image-flatten -c -i example/input -o example/combine.jpg`:
![Alt text](/example/combine.jpg?raw=true)

Using 'average' mode `image-flatten -a -i example/input -o example/average.jpg`:
![Alt text](/example/average.jpg?raw=true)


# Build

Run `stack build`

# Run

Run `image-flatten` with various options:

    image-flatten - flatten multiple images into a single image, combining or hiding
    differences

    Usage: image-flatten (-i|--input DIRECTORY) (-o|--output OUTPUT) [-c|--combine]
                         [-r|--remove] [-a|--average] [-t|--threshold ARG]
                         [-q|--quality ARG]
      Flatten DIRECTORY into OUTPUT

    Available options:
      -h,--help                Show this help text
      -i,--input DIRECTORY     A directory containing images to flatten
      -o,--output OUTPUT       File path to save result image to
      -c,--combine             Specifies that differences in images should be
                               combined
      -r,--remove              Specifies that differences in images should be
                               removed
      -a,--average             Specifies that images should be averaged
      -t,--threshold ARG       Adjust the sensitivity for detecting features. A low
                               number is required to detect subtle differences eg a
                               green jumper on grass. A high number will suffice for
                               white stars against a black sky. Default is 10.
      -q,--quality ARG         If output is a JPEG, this specifies the quality to
                               use when saving
