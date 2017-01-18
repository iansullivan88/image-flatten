A tool for flattening multiple slightly different images into a single image. There are two modes. The first is to remove variations for example if you take 10 identical picture of a landscape as a person walked across it, the program would attempt to remove the person, resulting in an image that only showed the landscape. The second mode is to combine all variations between pictures - in the previous example this would produce an image that contained the landscape as well as 10 copies of the person.

# Build

Run `stack build`

# Run

Run `image-flatten` with various options:

    image-flatten - flatten multiple images into a single image, combining or hiding
    differences

    Usage: image-flatten (-i|--input DIRECTORY) (-o|--output OUTPUT) [-c|--combine]
                         [-r|--remove] [-t|--threshold ARG] [-q|--quality ARG]
      Flatten DIRECTORY into OUTPUT

    Available options:
      -h,--help                Show this help text
      -i,--input DIRECTORY     A directory containing images to flatten
      -o,--output OUTPUT       File path to save result image to
      -c,--combine             Specifies that differences in photos should be
                               combined
      -r,--remove              Specifies that differences in photos should be
                               removed
      -t,--threshold ARG       Adjust the sensitivity for detecting features. A low
                               number is required to detect subtle differences eg a
                               green jumper on grass. A high number will suffice for
                               white stars against a black sky. Default is 10.
      -q,--quality ARG         If output is a JPEG, this specifies the quality to
                               use when saving
