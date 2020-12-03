#include <gst/gst.h>
#include <stdio.h>
#include <stdlib.h>

static void
cb_new_pad (GstElement *element,
        GstPad     *pad,
        gpointer    data)
{
  gchar *name;

  name = gst_pad_get_name (pad);
  g_print ("A new pad %s was created\n", name);
  g_free (name);

  /* here, you would setup a new pad link for the newly created pad */
}

int 
main (int   argc,
      char *argv[]) 
{
  GstElement *pipeline, *source, *demux;
  GMainLoop *loop;

  if (argc != 2) {
      fprintf(stderr, "Missing ogg file.\n");
      exit(1);
  }

  /* init */
  gst_init (&argc, &argv);

  /* create elements */
  pipeline = gst_pipeline_new ("my_pipeline");
  source = gst_element_factory_make ("filesrc", "source");
  g_object_set (source, "location", argv[1], NULL);
  demux = gst_element_factory_make ("oggdemux", "demuxer");

  /* you would normally check that the elements were created properly */
  printf("loc is %s, source is %p, %p, %p\n", argv[1], source, pipeline, demux);
  if (demux == NULL) {
      fprintf(stderr, "Couldn't make oggdemux. Probably missing ogg plug-in. If on OS X using homebrew\n"
              "see available options using: brew info gst-plugins-base\n");
      exit(1);
  }

  /* put together a pipeline */
  gst_bin_add_many (GST_BIN (pipeline), source, demux, NULL);
  gst_element_link_pads (source, "src", demux, "sink");

  /* listen for newly created pads */
  g_signal_connect (demux, "pad-added", G_CALLBACK (cb_new_pad), NULL);

  /* start the pipeline */
  gst_element_set_state (GST_ELEMENT (pipeline), GST_STATE_PLAYING);
  loop = g_main_loop_new (NULL, FALSE);
  g_main_loop_run (loop);

  printf("done\n");
  return 0;

}
