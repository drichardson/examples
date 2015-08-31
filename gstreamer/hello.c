// http://gstreamer.freedesktop.org/data/doc/gstreamer/head/manual/html/chapter-helloworld.html

#include <gst/gst.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>

static gboolean
bus_call(GstBus *bus, GstMessage *msg, gpointer data) {
    GMainLoop *loop = (GMainLoop*)data;

    switch(GST_MESSAGE_TYPE(msg)) {
    case GST_MESSAGE_EOS:
        g_print("End of stream\n");
        g_main_loop_quit(loop);
        break;

    case GST_MESSAGE_ERROR: {
        gchar *debug;
        GError *error;
        gst_message_parse_error(msg, &error, &debug);

        g_free(debug);
        
        g_printerr("Error: %s\n", error->message);
        g_error_free(error);

        g_main_loop_quit(loop);
        break;
    }

    default:
        break;
    }

    return TRUE;
}

static void
on_pad_added(GstElement *element, GstPad *pad, gpointer data) {
    GstPad *sinkpad;
    GstElement* decoder = (GstElement*)data;

    g_print("Dynamic pad created, linking demuxer/decoder\n");

    sinkpad = gst_element_get_static_pad(decoder, "sink");
    gst_pad_link(pad, sinkpad);
    gst_object_unref(sinkpad);
}

int
main(int argc, char** argv) {
    gst_init(&argc, &argv);

    GMainLoop *loop = g_main_loop_new(NULL, FALSE);
    
    if (argc != 2) {
        g_printerr("Usage: %s <Ogg/Vorbis filename>\n", argv[0]);
        exit(1);
    }

    GstElement *pipeline = gst_pipeline_new("audio-player");
    GstElement *source = gst_element_factory_make("filesrc", "file-source");
    GstElement *demuxer = gst_element_factory_make("oggdemux", "demuxer");
    GstElement *decoder = gst_element_factory_make("vorbisdec", "decoder");
    GstElement *converter = gst_element_factory_make("audioconvert", "converter");
    GstElement *sink = gst_element_factory_make("autoaudiosink", "audio-output");

    if (!pipeline || !source || !demuxer || !decoder || !converter || !sink) {
        g_printerr("An element could not be created %p, %p, %p, %p, %p, %p.", pipeline, source, demuxer, decoder, converter, sink);
        exit(1);
    }

    g_object_set(source, "location", argv[1], NULL);


    GstBus * bus = gst_pipeline_get_bus(GST_PIPELINE(pipeline));
    guint bus_watch_id = gst_bus_add_watch(bus, bus_call, loop);

    gst_bin_add_many(GST_BIN(pipeline), source, demuxer, decoder, converter, sink, NULL);
    gst_element_link(source, demuxer);
    gst_element_link_many(decoder, converter, sink, NULL);
    g_signal_connect(demuxer, "pad-added", G_CALLBACK(on_pad_added), decoder);

    g_print("Now playing...\n");
    gst_element_set_state(pipeline, GST_STATE_PLAYING);

    g_print("Running...\n");
    g_main_loop_run(loop);

    g_print("Returned, stopping playback\n");
    gst_element_set_state(pipeline, GST_STATE_NULL);

    g_print("Deleting pipeline\n");
    gst_object_unref(pipeline);
    g_source_remove(bus_watch_id);
    g_main_loop_unref(loop);

    return 0;
}

