# Generate a test file 
gst-launch-1.0 videotestsrc ! theoraenc ! oggmux ! filesink location="test.ogg"

# Play test video in a window
gst-launch-1.0 videotestsrc ! autovideosink
